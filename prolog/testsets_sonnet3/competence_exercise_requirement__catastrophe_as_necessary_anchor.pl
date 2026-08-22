% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__catastrophe_as_necessary_anchor, []).

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
 *   constraint_id: competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   human_readable: Catastrophe-as-Necessary-Anchor Reading of the Competence Exercise Requirement
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates one specific reading of the contested 'competence
 *   exercise requirement' kernel in high-reliability organizations (aviation,
 *   nuclear, surgery, process safety): the claim that only genuine
 *   catastrophic events or near-misses provide the irreducible exercise
 *   needed to maintain operational competence, and that high-fidelity
 *   simulation, however sophisticated, cannot substitute for this. As
 *   catastrophe-free periods lengthen (a mark of success for the underlying
 *   safety system), this reading increasingly functions to devalue simulation
 *   investment, mark long-tenured catastrophe-free operators as having
 *   unverifiable competence, and sustain the jurisdictional relevance of
 *   credentialing and investigation bodies whose authority depends partly on
 *   the felt necessity of real-world anchoring. The rising theater_ratio
 *   reflects a growing gap between the reading's stated coordination function
 *   (preventing false confidence from rehearsal) and its increasing use as
 *   institutional cover for maintaining oversight authority and explaining
 *   away outcomes after the fact.
 *
 * KEY AGENTS:
 *   - veteran_operators_with_live_incident_experience: beneficiary via status/credibility
 *   - incident_investigation_bodies: agenda_setter via retrospective narrative authority
 *   - safety_credentialing_authorities: agenda_setter/beneficiary via sustained jurisdiction
 *   - frontline_operators_in_low_incident_eras: payer, cannot satisfy the standard through any available action
 *   - simulation_training_vendors: payer, product capped by definitional insufficiency
 *   - frontline_operators_during_the_actual_event: payer, bears acute cost, benefit accrues to institution
 *   - passengers_patients_and_public_bystanders: excluded, bears the risk the exercise requires
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.58).
domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.42).
domain_priors:theater_ratio(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__catastrophe_as_necessary_anchor, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Catastrophe-as-Necessary-Anchor Reading of the Competence Exercise Requirement").
narrative_ontology:topic_domain(competence_exercise_requirement__catastrophe_as_necessary_anchor, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__catastrophe_as_necessary_anchor, '22fab717-b6bb-4717-8061-c1d8f7164fd8').
narrative_ontology:cs_kernel_codification('22fab717-b6bb-4717-8061-c1d8f7164fd8', distributed).
narrative_ontology:cs_authority_grounding('22fab717-b6bb-4717-8061-c1d8f7164fd8', practice).
narrative_ontology:cs_interpretation_layer_present('22fab717-b6bb-4717-8061-c1d8f7164fd8').
narrative_ontology:cs_reading_relation('22fab717-b6bb-4717-8061-c1d8f7164fd8', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('22fab717-b6bb-4717-8061-c1d8f7164fd8', competence_exercise_requirement__hybrid_dependency, influences).
narrative_ontology:cs_axiom('22fab717-b6bb-4717-8061-c1d8f7164fd8', foundational, genuine_stakes_irreplicable_by_design).
narrative_ontology:cs_axiom_status(genuine_stakes_irreplicable_by_design, holdable).
narrative_ontology:cs_axiom_grounding('22fab717-b6bb-4717-8061-c1d8f7164fd8', genuine_stakes_irreplicable_by_design, empirically_contingent).
narrative_ontology:cs_axiom('22fab717-b6bb-4717-8061-c1d8f7164fd8', secondary, simulation_produces_bounded_confidence_ceiling).
narrative_ontology:cs_axiom_status(simulation_produces_bounded_confidence_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('22fab717-b6bb-4717-8061-c1d8f7164fd8', simulation_produces_bounded_confidence_ceiling, empirically_contingent).
narrative_ontology:cs_reference_frame('22fab717-b6bb-4717-8061-c1d8f7164fd8', post_incident_gap_discovery_era).
narrative_ontology:cs_drift_state('22fab717-b6bb-4717-8061-c1d8f7164fd8', high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('22fab717-b6bb-4717-8061-c1d8f7164fd8', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, veteran_operators_with_live_incident_experience).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, incident_investigation_bodies).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_credentialing_authorities).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators_in_low_incident_eras).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_training_vendors).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, organizations_that_invested_in_simulation_infrastructure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators_during_the_actual_event).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, muscle_memory_requires_real_stakes).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_fidelity_ceiling_exists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pilots, control-room operators, and surgeons who lived through a real catastrophic event or near-miss early in their careers hold status and internal credibility that colleagues without such exposure cannot match, regardless of simulator hours. Their authority within the organization is partly constituted by having 'been there,' and this reading of the competence kernel validates that authority as a genuine competence signal rather than survivorship or seniority.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, veteran_operators_with_live_incident_experience, beneficiary,
    organized, biographical, arbitrage, national).

% Bodies like the NTSB, chemical safety boards, and root-cause review panels write the retrospective narratives that decide whether a bad outcome is attributed to 'decayed competence despite simulation' or to bad luck, equipment failure, or process design. Their post-hoc framing power sets which reading of the kernel becomes institutionally dominant after each event.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, incident_investigation_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Regulatory and certification bodies that set recurrency requirements can point to catastrophe-anchored competence decay as justification for line-operations mandates, non-jeopardy audits, and continued relevance of their oversight function. A kernel that says simulation alone is adequate would shrink their jurisdiction; this reading sustains it.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_credentialing_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_credentialing_authorities, beneficiary).

% Operators who entered the field during long safety-record stretches with no real catastrophic exposure are, under this reading, permanently marked as having an unverifiable, possibly-decayed competence no amount of simulator time can certify. They cannot manufacture a real catastrophe to prove themselves, and their careers, promotions, and self-assessment are shadowed by a standard they structurally cannot meet through any action available to them.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators_in_low_incident_eras, payer,
    moderate, biographical, trapped, national).

% Companies that build high-fidelity simulators and debrief protocols find their entire value proposition structurally capped by this reading: no matter how good the simulation gets, it is declared definitionally insufficient. They must either contest the kernel directly (expensive, slow) or accept a permanent 'necessary but not sufficient' ceiling on their product's claimed value.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_training_vendors, payer,
    moderate, biographical, constrained, global).

% Airlines, hospitals, and utilities that built extensive simulation-based training programs to reduce dependence on live incidents now face a reading in which their investment is treated as necessary-but-inadequate, exposing them to liability findings ('should have provided more real-world exposure') even where their simulation programs meet or exceed industry design standards.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, organizations_that_invested_in_simulation_infrastructure, payer,
    powerful, generational, constrained, national).

% The operators physically present during a real catastrophic event or near-miss bear its full acute cost — injury risk, trauma, split-second decision burden — while the organizational and credentialing benefit of that exposure (the 'competence anchor') accrues afterward to the institution and to the operator's later career, not to them in the moment. Their voice on what the event actually revealed about competence is frequently absent from the retrospective narrative, which is authored by investigators and management.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators_during_the_actual_event, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators_during_the_actual_event, excluded).

% The people whose lives are put at risk during the 'necessary' catastrophic or near-miss event that this reading treats as an irreducible training input have no voice in whether their exposure to danger is an acceptable cost of maintaining institutional competence. They are the raw material of the exercise the reading declares indispensable.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, passengers_patients_and_public_bystanders, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_credentialing_authorities).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__catastrophe_as_necessary_anchor, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates organizational attention and resource allocation around a genuine problem: high-fidelity simulation may create false confidence ('knowing about' a failure mode) that does not transfer to the different cognitive and physiological state produced by genuine, irreversible-stakes events. The reading tries to keep organizations honest about the limits of rehearsal.
% TRANSFER_FUNCTION: Moves credibility, career advancement, and regulatory authority toward those who administer or possess catastrophe-derived experience, and moves liability exposure and unresolvable self-doubt onto those whose careers occurred during catastrophe-free periods, and onto simulation vendors whose product is declared structurally incomplete regardless of quality.
% ABSENT_VOICES: The operators physically present during the anchoring event, and the public exposed to its risk, are rarely the ones who get to say what the event proved about competence — that determination is made afterward by investigators, credentialing bodies, and senior colleagues interpreting the event as a data point in an institutional narrative they did not choose to generate.
% DISAPPEARANCE_RATIONALE: If this specific reading of the kernel vanished, credentialing bodies would lose one justification for line-operations mandates and non-jeopardy audits (though the hybrid_dependency reading could supply a similar one), simulation vendors would gain unambiguous standing to claim their product is sufficient, and low-incident-era operators would be relieved of a standard they cannot meet — but incident investigators would dispute that anything real changed, since they hold that catastrophe-derived competence decay is an empirical fact, not merely a convenient narrative.
% FOUNDING_PROBLEM: Early aviation, nuclear, and chemical-process safety cultures observed that operators who had never faced a real emergency sometimes froze, mis-prioritized, or failed to adapt during actual crises in ways their simulator records did not predict — the founding problem was the gap between rehearsed competence and competence-under-genuine-irreversible-stakes.
% FOUNDING_PROBLEM_CORROBORATION: Some incident investigators and cognitive-science researchers outside the credentialing bodies (e.g., independent human-factors researchers studying stress physiology and decision-making under genuine threat) corroborate that a real gap between simulated and lived competence exists. However, other researchers outside the benefiting parties — simulation-fidelity researchers and several human-factors labs — report that sufficiently realistic, surprise-injected simulation scenarios close most of the measured gap, undercutting the claim that only literal catastrophe can serve the function; no fully independent consensus exists.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__catastrophe_as_necessary_anchor, contested).
narrative_ontology:founding_problem_status(competence_exercise_requirement__catastrophe_as_necessary_anchor, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that under this reading, an entire cohort of operators is marked as having unverifiable competence through no fault or action of their own, and simulation vendors face a permanent, unfalsifiable ceiling on their claimed value regardless of demonstrated fidelity. Theater ratio is the most diagnostically important metric here and is authored rising over the interval (0.35 -> 0.61): the coordination function (catching simulation's false-confidence blind spot) is real but shrinking relative to the reading's growing use as institutional rhetoric — 'we need line time / non-jeopardy audits / continued oversight because simulation alone is known to be insufficient' becomes a self-sustaining justification independent of fresh evidence. Suppression is present but moderate (0.42): there is no direct coercive mechanism preventing operators or vendors from contesting the reading, but credentialing authority and post-incident narrative control create real asymmetry in whose account of competence prevails.
 *
 * DIRECTIONALITY LOGIC:
 *   Veteran operators with real incident exposure and the institutions whose authority depends on catastrophe-anchoring (investigation bodies, credentialing authorities) sit near the beneficiary end: the reading validates their existing status or expands their jurisdiction. Operators who happened to build careers during catastrophe-free eras, simulation vendors, and organizations that invested heavily in simulation infrastructure sit near the target end: the reading imposes a standard on them they structurally cannot satisfy or a ceiling they cannot break through performance. Operators present during the actual anchoring event and bystanders exposed to its risk are targets in the most literal sense — they bear the acute cost of the 'irreducible exercise' the reading declares necessary, while the durable benefit (institutional competence validation) accrues elsewhere and later.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — real emergencies sometimes reveal decision-making failures that simulator records did not predict — was live enough historically to be corroborated by human-factors researchers outside the credentialing bodies. But as simulation fidelity and surprise-injection methodology have advanced, other independent researchers report the gap closing substantially, which is exactly the founding_problem_status: contested signal this schema is built to surface. Treating the classification as tangled_rope rather than snare acknowledges the genuine coordination content (guarding against rehearsal-induced overconfidence is a real organizational hazard) while flagging that the reading's persistence increasingly serves credentialing-authority jurisdiction and veteran-status validation independent of whether the underlying empirical gap has narrowed — a mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    muscle_memory_vs_knowing_about_distinction,
    'Is there a genuine, empirically demonstrable cognitive/physiological difference between simulator-exercised competence (''knowing about'' a failure mode) and catastrophe-exercised competence (''muscle memory'' under genuine irreversible stakes), or is this distinction a folk-theoretic narrative that persists because it validates the status of catastrophe-experienced veterans and the jurisdiction of investigation/credentialing bodies?',
    'Controlled comparison of decision-quality and stress-physiology metrics (cortisol response, decision latency, error rates) between operators with real catastrophic exposure and operators with only high-fidelity, surprise-injected simulation exposure, matched for tenure and training hours, across multiple real subsequent events.',
    'If no measurable difference exists once simulation fidelity is controlled for, this reading is a status-preserving narrative rather than a competence claim and the classification shifts toward snare (the coordination story is cover). If a genuine, simulation-resistant gap is confirmed, the coordination function is more substantial and the classification is more legitimately tangled_rope or even a scaffold pending research resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(muscle_memory_vs_knowing_about_distinction, empirical, 'Whether catastrophe-derived competence is empirically distinct from well-designed simulation-derived competence.').

omega_variable(
    kernel_reading_selection_incentive,
    'Why does this particular reading (catastrophe as necessary, not merely useful) persist institutionally over the hybrid_dependency reading, which could satisfy the same underlying safety concern with lower cost to catastrophe-free-era operators and simulation vendors?',
    'Trace which institutional actors advocate for each reading in regulatory rulemaking comments, credentialing standard revisions, and post-incident report language; compare advocacy patterns against which reading best serves each actor''s structural position.',
    'If advocacy for the catastrophe_as_necessary_anchor reading correlates strongly with actors whose authority or status depends on it (investigation bodies, veteran operators) rather than with independent safety-outcome data, this supports classifying the reading itself as serving an extraction function beyond its coordination content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_incentive, conceptual, 'Whether reading-selection among the three kernel readings tracks institutional interest rather than safety evidence.').

omega_variable(
    public_risk_exposure_justification,
    'Does the acceptance of real catastrophic events or near-misses as a ''necessary'' competence-exercise input imply an institutional tolerance for maintaining conditions under which such events can recur, rather than driving those conditions toward elimination?',
    'Examine whether organizations holding this reading show measurably different investment patterns in hazard elimination versus organizations holding the simulation_as_adequate_exercise reading, controlling for industry and regulatory environment.',
    'If catastrophe-anchor-reading organizations show weaker hazard-elimination investment (because the catastrophe is quietly valued as a training input), this reading directly implicates the excluded public-bystander stakeholder and strengthens the case for treating theater_ratio as capturing genuine misalignment rather than benign narrative drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_risk_exposure_justification, preference, 'Whether this reading creates perverse incentives against hazard elimination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 8, 0.42).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 16, 0.49).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 24, 0.55).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 32, 0.59).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 40, 0.61).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(comp_be_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(comp_be_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(comp_be_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(comp_be_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(comp_su_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(comp_su_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 24, 0.39).
narrative_ontology:measurement(comp_su_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 32, 0.41).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__catastrophe_as_necessary_anchor, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.12).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, hybrid_dependency).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the competence_exercise_requirement kernel. simulation_as_adequate_exercise holds the opposite core premise (simulation suffices) and would carry a much lower extractiveness for simulation vendors and low-incident-era operators. hybrid_dependency occupies a middle position, requiring periodic real (but not necessarily catastrophic) anchoring, and would show a distinct beneficiary/victim structure again. Each reading is authored as its own constraint with its own stable ε per the ε-invariance principle; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
