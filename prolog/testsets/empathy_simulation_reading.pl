% ============================================================================
% CONSTRAINT STORY: empathy_simulation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_empathy_simulation_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: empathy_simulation_reading
 *   human_readable: Simulated Policy Advocacy as Empathy-Building Practice (Fiat Efficacy: Empathy Reading)
 *   domain: debate theory/political philosophy
 *
 * SUMMARY:
 *   In competitive policy debate, 'fiat' allows debaters to argue as if a
 *   hypothetical government action were enacted, without any actual policy
 *   consequence. Critics have long asked what justifies devoting years of
 *   adolescent effort to advocating for or against policies that will never
 *   be implemented by the debaters. One influential answer, distinct from
 *   claims about scholarship value, predictive accuracy, or truth-testing,
 *   locates the payoff entirely in the psychological transformation of the
 *   participant: simulating the perspective of a policy's beneficiaries or
 *   victims builds real empathy and attitude change that persists after the
 *   round ends and after the activity is left behind, independent of whether
 *   the policy itself has any chance of being enacted. This story generates
 *   ONLY that reading as its own ε-invariant constraint, per the kernel
 *   decomposition rule; the sibling readings (empirical precedent,
 *   scholarship value, truth-procedure, predictive synthesis, utopian
 *   fiction) are separate constraints with their own ε values and are not
 *   folded in here.
 *
 * KEY AGENTS:
 *   - competitive_debate_participants: primary claimed beneficiary (moderate/mobile) — undergoes the simulated perspective-taking
 *   - debate_coaches_and_programs: agenda_setter (organized/mobile) — designs and administers the activity's justificatory framework
 *   - fellow_citizens_encountered_in_deliberation: downstream claimed beneficiary (powerless/analytical) — receives whatever attitude change participants carry forward
 *   - skeptical_debate_theorists: excluded voice (moderate/mobile) — contests the empathy claim's testability from inside the field but rarely wins institutionally
 *   - external_social_psychology_researchers: observer (institutional/analytical) — could test the claim but mostly hasn't, specifically for this population
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(empathy_simulation_reading, 0.28).
domain_priors:suppression_score(empathy_simulation_reading, 0.15).
domain_priors:theater_ratio(empathy_simulation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(empathy_simulation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(empathy_simulation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(empathy_simulation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(empathy_simulation_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(empathy_simulation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(empathy_simulation_reading, rope).
narrative_ontology:human_readable(empathy_simulation_reading, "Simulated Policy Advocacy as Empathy-Building Practice (Fiat Efficacy: Empathy Reading)").
narrative_ontology:topic_domain(empathy_simulation_reading, "debate theory/political philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(empathy_simulation_reading, '2b6736c9-770c-48d9-b420-067d49ab10cc').
narrative_ontology:cs_kernel_codification('2b6736c9-770c-48d9-b420-067d49ab10cc', distributed).
narrative_ontology:cs_authority_grounding('2b6736c9-770c-48d9-b420-067d49ab10cc', practice).
narrative_ontology:cs_interpretation_layer_present('2b6736c9-770c-48d9-b420-067d49ab10cc').
narrative_ontology:cs_reading_relation('2b6736c9-770c-48d9-b420-067d49ab10cc', fiat_efficacy_kernel__empirical_precedent_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b6736c9-770c-48d9-b420-067d49ab10cc', fiat_efficacy_kernel__scholarship_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b6736c9-770c-48d9-b420-067d49ab10cc', fiat_efficacy_kernel__truth_procedure_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b6736c9-770c-48d9-b420-067d49ab10cc', fiat_efficacy_kernel__predictive_synthesis_reading, influences).
narrative_ontology:cs_reading_relation('2b6736c9-770c-48d9-b420-067d49ab10cc', fiat_efficacy_kernel__utopian_fiction_reading, coexists_with).
narrative_ontology:cs_axiom('2b6736c9-770c-48d9-b420-067d49ab10cc', foundational, efficacy_located_in_participant_psychology_not_enactment).
narrative_ontology:cs_axiom_status(efficacy_located_in_participant_psychology_not_enactment, holdable).
narrative_ontology:cs_axiom_grounding('2b6736c9-770c-48d9-b420-067d49ab10cc', efficacy_located_in_participant_psychology_not_enactment, empirically_contingent).
narrative_ontology:cs_axiom('2b6736c9-770c-48d9-b420-067d49ab10cc', secondary, policy_realism_irrelevant_to_value_of_simulation).
narrative_ontology:cs_axiom_status(policy_realism_irrelevant_to_value_of_simulation, holdable).
narrative_ontology:cs_axiom_grounding('2b6736c9-770c-48d9-b420-067d49ab10cc', policy_realism_irrelevant_to_value_of_simulation, instrumental).
narrative_ontology:cs_reference_frame('2b6736c9-770c-48d9-b420-067d49ab10cc', adversarial_perspective_taking_as_pedagogy).
narrative_ontology:cs_drift_state('2b6736c9-770c-48d9-b420-067d49ab10cc', contemporary_debate_pedagogy, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('2b6736c9-770c-48d9-b420-067d49ab10cc', '').
narrative_ontology:cs_kernel_id(empathy_simulation_reading, fiat_efficacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(empathy_simulation_reading, competitive_debate_participants).
narrative_ontology:constraint_beneficiary(empathy_simulation_reading, debate_coaches_and_programs).
narrative_ontology:constraint_beneficiary(empathy_simulation_reading, fellow_citizens_encountered_in_deliberation).
narrative_ontology:constraint_vindicates(empathy_simulation_reading, simulation_produces_attitude_change_independent_of_enactment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Argue hypothetical government or policy actions ('fiat') in competitive rounds, adopting perspectives of affected populations they do not belong to. The claimed payoff accrues to them directly: rehearsing an opposing or unfamiliar standpoint under adversarial pressure is asserted to produce measurable reductions in prejudice and durable perspective-taking skill, regardless of whether any simulated policy is ever enacted. They can leave the activity at graduation or choose not to compete; nothing traps them in it.
narrative_ontology:constraint_stakeholder(empathy_simulation_reading, competitive_debate_participants, beneficiary,
    moderate, biographical, mobile, regional).

% Design curricula and judging norms that reward argumentative engagement with fiated policy scenarios. They administer the practice, decide how much weight empathy/attitude outcomes receive relative to argumentative technique, and could redesign the activity around alternative justifications (predictive synthesis, scholarship value) if this one lost currency. Their institutional survival depends only loosely on this specific efficacy claim among several competing ones.
narrative_ontology:constraint_stakeholder(empathy_simulation_reading, debate_coaches_and_programs, agenda_setter,
    organized, generational, mobile, national).

% Not participants in debate rounds themselves, but the people whose perspectives participants simulate, and who participants later encounter in ordinary civic life carrying whatever attitude change the simulation produced. They have no voice in how the practice is run and cannot verify from outside whether the claimed empathy transfer actually reaches them, but they are the named downstream beneficiary of the mechanism if it works as claimed.
narrative_ontology:constraint_stakeholder(empathy_simulation_reading, fellow_citizens_encountered_in_deliberation, beneficiary,
    powerless, generational, analytical, local).

% Debate scholars who argue the empathy-transfer claim is unfalsifiable within the activity's own institutions (judges reward the performance of empathy claims, not verified attitude change) and who would prefer efficacy be located in argument quality, policy literacy, or truth-testing rather than psychological transformation. Their objection rarely surfaces in-round because ballots reward advocacy of the framework, not refutation of it.
narrative_ontology:constraint_stakeholder(empathy_simulation_reading, skeptical_debate_theorists, excluded,
    moderate, biographical, mobile, national).

% Study contact theory and perspective-taking interventions independent of debate institutions. They could, in principle, test whether competitive debate participation produces measurable prejudice reduction relative to controls, but very little of this research has actually been done inside competitive debate specifically — most citations debate theorists use come from adjacent psychology literature applied by analogy.
narrative_ontology:constraint_stakeholder(empathy_simulation_reading, external_social_psychology_researchers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(empathy_simulation_reading, diffuse).
narrative_ontology:fixing_cost_class(empathy_simulation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Structures adversarial rounds so participants must argue positions and simulate the standpoints of people materially different from themselves, under competitive incentive to do so persuasively and repeatedly.
% TRANSFER_FUNCTION: Moves cognitive and emotional effort from participants into rehearsed perspective-taking; the claimed return is internal (participant's own capacity for empathy, reduced in-group bias) rather than external policy change, and is asserted to flow onward to whoever participants later interact with as citizens.
% ABSENT_VOICES: Skeptical debate theorists who think the empathy claim is a convenient justification for an activity that mostly rewards technical argumentation are structurally present but rarely win the argument in-round, since ballots are adjudicated inside the same institution that benefits from the framework being accepted. External psychology researchers who could actually test the claim are mostly not consulted by debate institutions at all.
% DISAPPEARANCE_RATIONALE: If this specific efficacy justification vanished, debate programs would very likely continue to run fiat-based policy debate under a different justification (scholarship value, argumentative training, or the predictive-synthesis reading) — the activity's practical mechanics do not depend on this particular story being true. But participants and coaches who have built their pedagogical identity around 'debate builds empathy' would experience a genuine rearrangement of how the activity is defended and marketed, even if the rounds themselves looked identical.
% FOUNDING_PROBLEM: Competitive policy debate needed a way to justify requiring students to argue positions they may personally reject or have no standing to enact, especially fiated government action that no participant can actually cause to happen; the empathy-simulation account answers 'why is this valuable if the policy is never real' by relocating the value inside the participant.
% FOUNDING_PROBLEM_CORROBORATION: Debate coaches and pedagogy literature produced from within competitive debate attest the empathy-building function is real and central. Independent social psychology research on perspective-taking interventions exists and supports a general mechanism, but very little of it has been conducted specifically on competitive debate populations, so the corroboration from outside the benefiting institutions is thin and mostly analogical rather than direct.
narrative_ontology:disappearance_verdict(empathy_simulation_reading, contested).
narrative_ontology:founding_problem_status(empathy_simulation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(empathy_simulation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(empathy_simulation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(empathy_simulation_reading, 0.28, 'claude-sonnet-5', 'fiat_efficacy_kernel_2026_20260803_102258', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(empathy_simulation_reading_tests).
:- end_tests(empathy_simulation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28) because the activity is voluntary, participants are not trapped, and no one is coerced into believing the empathy claim to remain in the activity — but it is not negligible, because the claim functions rhetorically to defend requiring students to spend enormous effort on policies they have no stake in enacting, and that defensive function has real opportunity costs for participants who might prefer an activity justified on grounds they find more credible. Suppression is low (0.15): dissenting theorists exist and publish, and the claim is not defended by exclusion of alternative views, only by home-field institutional advantage. Theater ratio rises across the measured interval (0.25 to 0.40) because as the activity professionalizes and empathy-based framing becomes a recruiting and grant-justification tool, more of the claim's public defense is rhetorical restatement rather than fresh evidence — the underlying psychological mechanism is asserted more often than it is newly tested.
 *
 * PERSPECTIVAL GAP:
 *   From inside the debate institution (coaches, veteran participants), the claim reads as an obviously true description of lived experience — many participants self-report attitude change. From the excluded theorist seat and the observer seat, the same claim reads as empirically underdetermined: the self-reports are exactly what an institution invested in defending an unfalsifiable practice would produce, and the psychology literature it leans on was not built to test this specific population or mechanism. The engine's per-seat computation should reflect that gap without this story asserting which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Participants and the citizens they later encounter are declared beneficiaries because the entire mechanism, if real, redounds to their own capacities and their community's deliberative texture — there is no external policy target extracting value from them. Coaches and programs are agenda_setters who administer the framework but do not personally capture rents from it in the way an extractive agenda_setter would; their institutional stake is in the activity's continued legitimacy, not in siphoning value from participants. No victim group is named: the reading, taken on its own terms, describes a coordination mechanism (rehearsed perspective-taking) with a claimed positive externality, not an extraction structure. This is why gain_flow is authored as diffuse rather than naming any capturing seat, and fixing_cost is cheap — nothing structural prevents debate institutions from adopting a different efficacy justification if this one is discredited.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (justifying fiat-based debate against never-enacted policy) has not disappeared as a live pedagogical question, but this specific ANSWER to it (empathy transfer) could become detached from evidence while the institutional practice of asserting it continues unchanged — that is the classic mandatrophy risk pattern (rising theater_ratio with stable extractiveness) rather than acute extraction. The reading stays coordination-flavored as long as the claim remains testable in principle and coaches do not suppress the theorists who contest it; it would drift toward tangled_rope only if defending the claim required active suppression of the psychology-research disconfirmation route, which is not currently the case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empathy_transfer_testability,
    'Is the claimed empathy/attitude-change effect actually measurable and durable in competitive debate populations specifically, or is it an untested import from general contact-theory psychology applied by analogy?',
    'Longitudinal, controlled studies comparing debate participants to matched non-participant peers on validated prejudice/perspective-taking measures, conducted by researchers outside debate institutions.',
    'If the effect does not replicate in this specific population, the reading''s coordination claim collapses into pure institutional self-justification, which would push the metrics toward higher theater_ratio and potentially reclassify the constraint as piton (a justification maintained by inertia after its evidentiary basis eroded) rather than rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empathy_transfer_testability, empirical, 'Whether the empathy-transfer mechanism is empirically established for this population or merely asserted by analogy.').

omega_variable(
    self_report_confound,
    'Do participant self-reports of attitude change reflect genuine internal transformation, or social-desirability bias produced by an institutional culture that rewards claiming to have been changed by the activity?',
    'Behavioral or implicit-measure studies that do not rely on participant self-report, compared against self-report data from the same cohort.',
    'A large gap between self-report and behavioral measures would suggest the claim functions partly as institutional theater rather than a real coordination mechanism, raising the effective theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_report_confound, empirical, 'Whether self-reported empathy gains are confounded by institutional social desirability.').

omega_variable(
    kernel_reading_choice_ambiguity,
    'When a debate coach or theorist defends ''fiat is efficacious,'' are they actually asserting THIS reading, or unconsciously blending it with the predictive_synthesis_reading or scholarship_reading in a way that makes the claim harder to falsify than any single reading alone?',
    'Discourse analysis of debate pedagogy literature and coaching materials to see whether efficacy claims cite psychological mechanisms specifically, or shift among justifications when challenged.',
    'If practitioners systematically equivocate among readings, the empathy_simulation_reading in practice may be doing less argumentative work than it appears, with the harder-to-test scholarship_reading absorbing the defensive burden — this would not change this story''s own ε but would affect how much weight to place on this reading''s real-world prevalence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_choice_ambiguity, conceptual, 'Whether the empathy reading is cleanly separable in practice from sibling readings when practitioners defend fiat debate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(empathy_simulation_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empa_tr_t0, empathy_simulation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(empa_tr_t5, empathy_simulation_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement(empa_tr_t10, empathy_simulation_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(empa_tr_t15, empathy_simulation_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(empa_tr_t20, empathy_simulation_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(empa_tr_t25, empathy_simulation_reading, theater_ratio, 25, 0.4).

% Extraction over time
narrative_ontology:measurement(empa_be_t0, empathy_simulation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(empa_be_t5, empathy_simulation_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(empa_be_t10, empathy_simulation_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(empa_be_t15, empathy_simulation_reading, base_extractiveness, 15, 0.27).
narrative_ontology:measurement(empa_be_t20, empathy_simulation_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(empa_be_t25, empathy_simulation_reading, base_extractiveness, 25, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(empathy_simulation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(empathy_simulation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(empathy_simulation_reading, 0.1).
narrative_ontology:affects_constraint(empathy_simulation_reading, empirical_precedent_reading).
narrative_ontology:affects_constraint(empathy_simulation_reading, scholarship_reading).
narrative_ontology:affects_constraint(empathy_simulation_reading, truth_procedure_reading).
narrative_ontology:affects_constraint(empathy_simulation_reading, predictive_synthesis_reading).
narrative_ontology:affects_constraint(empathy_simulation_reading, utopian_fiction_reading).

% DUAL FORMULATION NOTE:
% This story is one of six sibling readings of fiat_efficacy_kernel, each with a distinct efficacy mechanism and distinct ε. empathy_simulation_reading locates value in participant/community psychological transformation and is authored as low-moderate extractiveness (0.28), consistent with a genuinely voluntary coordination activity whose defensive claim is contestable but not coercively maintained. It should not be merged with predictive_synthesis_reading (which claims a different, more falsifiable payoff structure external to the participant) or scholarship_reading (which locates value in research output rather than attitude change). Contamination propagation: if empirical evidence strongly disconfirms the empathy mechanism, defenders of fiat debate are structurally likely to shift weight onto predictive_synthesis_reading or scholarship_reading, which is why this story marks an influences edge toward predictive_synthesis_reading specifically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
