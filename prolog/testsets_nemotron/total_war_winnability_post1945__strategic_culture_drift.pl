% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Total War Winnability (Post-1945) — Strategic Culture Drift Reading
 *   domain: international_relations/strategic_studies/commitment_system
 *
 * SUMMARY:
 *   This constraint story instantiates the strategic_culture_drift reading of
 *   the total_war_winnability_post1945 kernel. The kernel is the question:
 *   'Is total war still winnable/reachable after 1945?' Three readings
 *   coexist: (1) normative_reading_drop — total war became normatively
 *   illegitimate; (2) structural_contraction_reading — nuclear weapons
 *   physically removed it from reachable space; (3) strategic_culture_drift —
 *   total war remains physically reachable but dropped from elite discourse
 *   via ideational shift. This reading treats the constraint as a Piton: a
 *   formerly functional coordination mechanism (the limited-war paradigm)
 *   whose primary function has atrophied but persists via institutional
 *   forgetting and theatrical maintenance of the paradigm. The beneficiary is
 *   defense intellectuals invested in limited-war frameworks; the victim is
 *   strategic flexibility (the atrophied capability to think and plan for
 *   conventional total war).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.42).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.31).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.42).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.19).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Total War Winnability (Post-1945) — Strategic Culture Drift Reading").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations/strategic_studies/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, '6b4501d1-11f9-46a5-8e0b-541635b73ed5').
narrative_ontology:cs_kernel_codification('6b4501d1-11f9-46a5-8e0b-541635b73ed5', distributed).
narrative_ontology:cs_authority_grounding('6b4501d1-11f9-46a5-8e0b-541635b73ed5', practice).
narrative_ontology:cs_interpretation_layer_present('6b4501d1-11f9-46a5-8e0b-541635b73ed5').
narrative_ontology:cs_reading_relation('6b4501d1-11f9-46a5-8e0b-541635b73ed5', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_reading_relation('6b4501d1-11f9-46a5-8e0b-541635b73ed5', total_war_winnability_post1945__structural_contraction_reading, forecloses).
narrative_ontology:cs_axiom('6b4501d1-11f9-46a5-8e0b-541635b73ed5', foundational, conventional_total_war_physically_reachable).
narrative_ontology:cs_axiom_status(conventional_total_war_physically_reachable, holdable).
narrative_ontology:cs_axiom_grounding('6b4501d1-11f9-46a5-8e0b-541635b73ed5', conventional_total_war_physically_reachable, empirically_contingent).
narrative_ontology:cs_axiom('6b4501d1-11f9-46a5-8e0b-541635b73ed5', foundational, discursive_drop_is_ideational_not_normative).
narrative_ontology:cs_axiom_status(discursive_drop_is_ideational_not_normative, holdable).
narrative_ontology:cs_axiom_grounding('6b4501d1-11f9-46a5-8e0b-541635b73ed5', discursive_drop_is_ideational_not_normative, conventional).
narrative_ontology:cs_reference_frame('6b4501d1-11f9-46a5-8e0b-541635b73ed5', limited_war_paradigm_sufficiency).
narrative_ontology:cs_drift_state('6b4501d1-11f9-46a5-8e0b-541635b73ed5', post_cold_war_peer_competition_return, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6b4501d1-11f9-46a5-8e0b-541635b73ed5', '2026-08-04T14:22:17Z').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals_limited_war).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, strategic_flexibility).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__strategic_culture_drift, limited_war_paradigm_sufficiency).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__strategic_culture_drift, escalation_control_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academic, think-tank, and uniformed intellectuals whose professional identities, funding streams, and career structures are organized around limited-war frameworks (escalation management, flexible response, counterinsurgency). They benefit from a discourse that treats total war as obsolete rather than latent — their expertise is the exclusive currency of the dominant paradigm. Exit is mobile: they could pivot to total-war scholarship, but the professional infrastructure rewards the limited-war specialization.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals_limited_war, beneficiary,
    institutional, generational, mobile, global).

% The capacity of state decision-makers to conceive, plan for, and if necessary execute total-war options when limited war fails or is inappropriate. This is not an actor but a structural capability — the strategic option-space that atrophies when elite discourse treats total war as conceptually unavailable. It pays the cost of institutional forgetting: if a crisis demands total-war logic (e.g., existential conventional conflict against a peer), the vocabulary, planning substrates, and organizational memory to execute it have eroded.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, strategic_flexibility, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(total_war_winnability_post1945__strategic_culture_drift, strategic_flexibility).

% The institutional complex (commands, labs, doctrinal shops, alliance staffs) that maintains nuclear deterrence as the primary strategic grammar. It administers the constraint by defining the boundary of thinkable conflict — nuclear use remains the only total-war scenario that receives serious planning, which reinforces the discursive drop of conventional total war. Exit is constrained: the establishment's bureaucratic logic and budgetary imperatives lock it into the nuclear-deterrence frame.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, nuclear_establishment, agenda_setter,
    institutional, generational, constrained, global).

% Scholars and practitioners who argue that conventional total war remains reachable and that the discursive taboo creates dangerous blind spots. They are structurally excluded from mainstream strategic doctrine, major funding lines, and official wargaming — their position is treated as anachronistic or alarmist. Exit is constrained: they can publish in niche venues but cannot shift the dominant paradigm without institutional sponsorship.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, revisionist_strategists, excluded,
    moderate, biographical, constrained, global).

% Strategic planning establishments of peer competitors (e.g., Russian General Staff, PLA) who observe the Western discursive drop of total war and may exploit it. They do not participate in the Western constraint but their planning assumptions are shaped by it — they may calculate that Western elites will not escalate to conventional total war, creating a permissive environment for limited aggression. Their seat is analytical: they model the constraint's effects without being subject to its internal enforcement.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, peer_competitor_planners, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates strategic discourse and planning around a shared limited-war paradigm, enabling alliance interoperability, crisis management vocabularies, and budgetary predictability — a common grammar for 'war below the nuclear threshold.'
% TRANSFER_FUNCTION: Moves strategic attention, intellectual capital, and institutional resources FROM conventional total-war planning TO limited-war frameworks (escalation ladders, counterinsurgency, precision strike, hybrid warfare). The transfer is not a single transaction but a decades-long reallocation of the strategic imagination.
% ABSENT_VOICES: Revisionist strategists who argue total war remains reachable are excluded from doctrine-setting venues. More fundamentally, future decision-makers who may face a crisis where limited-war logic fails are absent — they cannot object to the atrophy of a capability they have not yet needed.
% DISAPPEARANCE_RATIONALE: If the discursive taboo on conventional total war vanished overnight, war colleges would reintroduce total-war modules, wargames would stress-test conventional mobilization at scale, procurement would hedge for industrial surge capacity, and alliance planning would reopen the conventional existential-war branch. The strategic option-space would expand measurably within 5-10 years.
% FOUNDING_PROBLEM: Post-1945, the founding problem was cognitive: how to think about war when nuclear weapons made total war potentially self-destructive? The limited-war paradigm solved this by bracketing nuclear use and creating a thinkable space for conventional conflict.
% FOUNDING_PROBLEM_CORROBORATION: The limited-war paradigm's founders (Kahn, Brodie, Kissinger, Schelling) explicitly framed their work as a response to the nuclear revolution — this is documented in their own writings and the institutional histories of RAND, the National Security Council, and the service war colleges. However, the claim that this paradigm remains adequate for the current strategic environment is contested by revisionist scholars (e.g., Colin Gray, Brad Roberts, recent CSIS/MITRE wargame teams) who are outside the beneficiary set and argue the founding problem has mutated: nuclear deterrence and conventional total war are not substitutes but adjacent rungs.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__strategic_culture_drift, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).
:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate: the constraint extracts strategic option-value from the system by narrowing the thinkable, but the extraction is diffuse and cumulative rather than concentrated. Suppression (0.31) is low-moderate: the constraint operates through discursive marginalization and curriculum design, not coercion — revisionist voices are not silenced, just defunded and deplatformed. Theater ratio (0.68) is high: wargames, doctrine publications, and strategic reviews perform the limited-war paradigm with increasing ritual fidelity while the underlying planning substrate for total war erodes. Accessibility collapse (0.28) is low: alternatives (total-war planning) are not structurally blocked, just unfashionable and institutionally unsupported. Resistance (0.19) is very low: the constraint meets little active resistance because its operation is invisible to most participants — it feels like 'how serious people think,' not a constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the defense intellectual seat, the constraint is a rope: a genuine coordination solution that enables alliance interoperability and crisis management. From the strategic flexibility seat, it is a piton: the coordination function (limited-war grammar) persists theatrically while the total-war capability it was meant to complement has atrophied. From the nuclear establishment seat, it is a mountain: nuclear deterrence makes conventional total war obsolete, so the discursive drop reflects structural reality. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Defense intellectuals (beneficiary) have institutional power and mobile exit — they collect professional rents from the paradigm. Strategic flexibility (payer) is a structural capability with civilizational time-horizon and trapped exit — it cannot 'exit' the atrophy, it simply ceases to exist. The nuclear establishment (agenda_setter) administers the constraint with constrained exit — its bureaucratic logic locks it to the nuclear-deterrence frame. Revisionist strategists (excluded) have moderate power but constrained exit — they see the structure but cannot shift it. Peer competitor planners (observer) have analytical exit — they model the constraint from outside.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding mandate — creating a thinkable space for conventional conflict under nuclear shadow — is partially live (nuclear deterrence still structures the strategic environment) but partially dead (the limited-war paradigm has become a ceiling rather than a floor). The Mandy trophy resolution is contested: the paradigm solved the 1945 problem but now creates a 2025 problem by foreclosing conventional total-war planning that peer competitors may not share.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discursive_vs_structural_causality,
    'Is the drop of total war from elite discourse the cause of strategic atrophy, or a symptom of a deeper structural shift (nuclear revolution, economic interdependence, precision strike) that made total war genuinely less relevant?',
    'Counterfactual analysis: if discourse had maintained total-war planning while material conditions evolved identically, would the capability have been preserved? Comparative study of states that retained total-war discourse (e.g., Soviet Cold War posture) vs. those that dropped it.',
    'If discursive drop is causal, the constraint is extractive (Piton) and reversible by discourse change. If it is symptomatic, the constraint is closer to Mountain — the discursive shift reflects an underlying structural contraction that discourse cannot reverse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discursive_vs_structural_causality, conceptual, 'Whether the ideational shift drives atrophy or merely reflects it.').

omega_variable(
    peer_competitor_asymmetry,
    'Do peer competitors (Russia, China) actually retain conventional total-war planning capability, or have they also undergone similar discursive atrophy masked by different rhetorical forms?',
    'Open-source analysis of Russian and Chinese war college curricula, wargame scenarios, mobilization doctrines, and procurement patterns for signatures of conventional total-war planning vs. limited-war optimization.',
    'If competitors retain the capability, the Western constraint creates a genuine strategic asymmetry (victim = strategic flexibility is real). If competitors have also atrophied, the constraint may be a shared civilizational drift — the victim is universal, not Western-specific.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peer_competitor_asymmetry, empirical, 'Whether the strategic flexibility victim is Western-specific or universal.').

omega_variable(
    kernel_reading_boundary,
    'Where exactly does the strategic_culture_drift reading foreclose, coexist with, or influence the normative_reading_drop and structural_contraction_reading?',
    'Structural mapping of each reading''s core premises: normative reading requires normative illegitimacy as primary driver; structural reading requires physical impossibility; drift reading requires physical reachability + discursive drop. Test pairwise logical compatibility.',
    'Determines the reading_relations in cs_structure. If drift reading forecloses structural reading (physical reachability vs. physical impossibility), the kernel has a genuine foreclosure pair. If all three coexist, the kernel is a live three-way dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical boundary between the three kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tw_winnability_strat_culture_tr_t1945, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(tw_winnability_strat_culture_tr_t1955, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1955, 0.28).
narrative_ontology:measurement(tw_winnability_strat_culture_tr_t1965, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1965, 0.42).
narrative_ontology:measurement(tw_winnability_strat_culture_tr_t1975, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1975, 0.51).
narrative_ontology:measurement(tw_winnability_strat_culture_tr_t1985, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1985, 0.58).
narrative_ontology:measurement(tw_winnability_strat_culture_tr_t1995, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1995, 0.62).
narrative_ontology:measurement(tw_winnability_strat_culture_tr_t2005, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2005, 0.65).
narrative_ontology:measurement(tw_winnability_strat_culture_tr_t2015, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2015, 0.67).
narrative_ontology:measurement(tw_winnability_strat_culture_tr_t2025, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2025, 0.68).

% Extraction over time
narrative_ontology:measurement(tw_winnability_strat_culture_be_t1945, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1945, 0.12).
narrative_ontology:measurement(tw_winnability_strat_culture_be_t1955, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1955, 0.18).
narrative_ontology:measurement(tw_winnability_strat_culture_be_t1965, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1965, 0.24).
narrative_ontology:measurement(tw_winnability_strat_culture_be_t1975, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1975, 0.29).
narrative_ontology:measurement(tw_winnability_strat_culture_be_t1985, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1985, 0.33).
narrative_ontology:measurement(tw_winnability_strat_culture_be_t1995, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1995, 0.36).
narrative_ontology:measurement(tw_winnability_strat_culture_be_t2005, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2005, 0.39).
narrative_ontology:measurement(tw_winnability_strat_culture_be_t2015, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement(tw_winnability_strat_culture_be_t2025, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(tw_winnability_strat_culture_su_t1945, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1945, 0.08).
narrative_ontology:measurement(tw_winnability_strat_culture_su_t1955, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1955, 0.15).
narrative_ontology:measurement(tw_winnability_strat_culture_su_t1965, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1965, 0.22).
narrative_ontology:measurement(tw_winnability_strat_culture_su_t1975, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1975, 0.28).
narrative_ontology:measurement(tw_winnability_strat_culture_su_t1985, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1985, 0.3).
narrative_ontology:measurement(tw_winnability_strat_culture_su_t1995, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1995, 0.31).
narrative_ontology:measurement(tw_winnability_strat_culture_su_t2005, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2005, 0.31).
narrative_ontology:measurement(tw_winnability_strat_culture_su_t2015, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2015, 0.31).
narrative_ontology:measurement(tw_winnability_strat_culture_su_t2025, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2025, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__strategic_culture_drift, identity_coordination).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__strategic_culture_drift, 0.08).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, nuclear_deterrence_posture).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, limited_war_doctrine).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, conventional_mobilization_planning).

% DUAL FORMULATION NOTE:
% Part of the total_war_winnability_post1945 constraint family. This reading (strategic_culture_drift) treats the constraint as Piton-class atrophy via institutional forgetting. The normative_reading_drop treats it as Snare-class normative suppression. The structural_contraction_reading treats it as Mountain-class physical impossibility. The three stories share the kernel but instantiate different constraints with different ε values and different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_winnability_post1945__strategic_culture_drift, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
