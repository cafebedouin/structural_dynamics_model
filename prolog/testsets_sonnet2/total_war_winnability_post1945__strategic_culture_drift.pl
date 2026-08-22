% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Total War as Discursively Abandoned Option in Post-1945 Strategic Culture
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This story instantiates the strategic-culture-drift reading of the
 *   total_war_winnability_post1945 kernel: total war remains physically and
 *   politically reachable (unlike the structural_contraction_reading, which
 *   holds nuclear weapons removed it from the reachable space entirely), but
 *   it dropped out of elite strategic discourse through an ideational shift —
 *   a generational and institutional migration of attention, prestige, and
 *   doctrinal effort toward limited-war and crisis-management frameworks.
 *   Unlike the normative_reading_drop (which locates the change in the
 *   delegitimization of total war under international humanitarian law and
 *   Article 2(4)), this reading locates the change in strategic culture
 *   itself: what defense intellectuals and general staffs find analytically
 *   tractable, careerially rewarding, and institutionally supported. The
 *   result is piton-class: an atrophied planning capacity maintained mostly
 *   by institutional habit and theatrical doctrine review rather than genuine
 *   readiness, with defense intellectuals invested in limited-war frameworks
 *   as the concentrated beneficiary and strategic flexibility as the diffuse
 *   victim.
 *
 * KEY AGENTS:
 *   - limited_war_defense_intellectuals: institutional/identity_locked — career and doctrinal capital tied to limited-war frameworks remaining dominant
 *   - arms_control_epistemic_community: organized/constrained — institutional relevance depends on total-war discourse retirement
 *   - strategic_planning_flexibility: institutional/trapped — the atrophied capacity itself, bearing the cost of disuse
 *   - peer_conflict_deterrence_credibility: moderate/constrained — deterrence signaling weakened by discursive retirement
 *   - revisionist_peer_states: institutional/analytical — excluded from the discourse shift, may not share its premises
 *   - strategic_studies_scholars: analytical/analytical — observe and distinguish this reading from its siblings
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
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Total War as Discursively Abandoned Option in Post-1945 Strategic Culture").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations/strategic_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, '916e73e7-511f-4c90-8b3d-d89ea3e90e2e').
narrative_ontology:cs_kernel_codification('916e73e7-511f-4c90-8b3d-d89ea3e90e2e', distributed).
narrative_ontology:cs_authority_grounding('916e73e7-511f-4c90-8b3d-d89ea3e90e2e', practice).
narrative_ontology:cs_interpretation_layer_present('916e73e7-511f-4c90-8b3d-d89ea3e90e2e').
narrative_ontology:cs_reading_relation('916e73e7-511f-4c90-8b3d-d89ea3e90e2e', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('916e73e7-511f-4c90-8b3d-d89ea3e90e2e', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_axiom('916e73e7-511f-4c90-8b3d-d89ea3e90e2e', foundational, reachability_is_ideational_not_physical).
narrative_ontology:cs_axiom_status(reachability_is_ideational_not_physical, holdable).
narrative_ontology:cs_axiom_grounding('916e73e7-511f-4c90-8b3d-d89ea3e90e2e', reachability_is_ideational_not_physical, empirically_contingent).
narrative_ontology:cs_axiom('916e73e7-511f-4c90-8b3d-d89ea3e90e2e', foundational, strategic_culture_shapes_planning_horizons_independent_of_law_or_capability).
narrative_ontology:cs_axiom_status(strategic_culture_shapes_planning_horizons_independent_of_law_or_capability, holdable).
narrative_ontology:cs_axiom_grounding('916e73e7-511f-4c90-8b3d-d89ea3e90e2e', strategic_culture_shapes_planning_horizons_independent_of_law_or_capability, empirically_contingent).
narrative_ontology:cs_reference_frame('916e73e7-511f-4c90-8b3d-d89ea3e90e2e', early_cold_war_massive_retaliation_doctrine).
narrative_ontology:cs_drift_state('916e73e7-511f-4c90-8b3d-d89ea3e90e2e', contemporary_great_power_competition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('916e73e7-511f-4c90-8b3d-d89ea3e90e2e', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, arms_control_epistemic_community).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, strategic_planning_flexibility).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, peer_conflict_deterrence_credibility).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__strategic_culture_drift, escalation_management_doctrine).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__strategic_culture_drift, graduated_response_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Built careers, institutional positions, and entire subfields (escalation ladders, graduated deterrence, crisis stability theory) on the premise that great-power total war is off the table conceptually, not merely deterred by capability. Their professional standing, publication records, and advisory influence depend on limited-war frameworks remaining the dominant lens through which planners and policymakers think about major conflict. Exiting this framework would devalue decades of accumulated intellectual capital.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals, agenda_setter).

% Treaty negotiators, verification specialists, and academic arms-control networks whose institutional relevance depends on total war being treated as a discourse to be managed away rather than a live planning contingency. They gain funding, access, and prestige from the framing that strategic culture has 'moved past' total-war thinking.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, arms_control_epistemic_community, beneficiary,
    organized, generational, constrained, global).

% Represents the institutional capacity of general staffs and national security planning bodies to seriously war-game and prepare doctrine for total war scenarios against peer adversaries. This capacity has atrophied as institutional attention, curricula, and career incentives shifted toward limited-war and gray-zone frameworks. It cannot 'exit' the constraint because it is not an actor with agency — it is the diminished planning capability itself, degraded by decades of institutional forgetting.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, strategic_planning_flexibility, payer,
    institutional, civilizational, trapped, global).

% The credibility of deterrent threats against a peer adversary contemplating large-scale conventional or hybrid aggression, insofar as that credibility depends on the adversary believing total war remains a genuinely considered option in the defender's strategic repertoire. Where elite discourse has discursively retired total-war thinking, adversaries may discount the plausibility of escalation to total war, weakening deterrence signaling even though the physical and political capacity for such escalation has not vanished.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, peer_conflict_deterrence_credibility, payer,
    moderate, generational, constrained, global).

% States contemplating aggression against the incumbent order are affected by whether they believe total war remains a live option for their adversaries, but they are not participants in the Western strategic-culture conversation that produced this discursive drop. Their own strategic cultures may not share the same ideational retirement of total-war thinking, creating an asymmetry the excluded voice would flag if consulted.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, revisionist_peer_states, excluded,
    institutional, generational, analytical, global).

% Historians and theorists of strategic culture who study how ideational shifts, generational turnover in officer corps and academia, and institutional path dependence caused total-war planning to fall out of elite discourse, distinguishing this ideational account from the rival structural (nuclear-impossibility) and normative (legal-illegitimacy) explanations for the same observed behavior.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, strategic_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__strategic_culture_drift, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates elite defense-intellectual and policymaking attention around a shared, tractable vocabulary (escalation control, limited aims, graduated response) that makes crisis management and arms negotiation analytically possible — total-war planning is combinatorially and politically unmanageable as a routine planning object, so the field has organized itself around scenarios it can actually model, brief, and negotiate over.
% TRANSFER_FUNCTION: Moves institutional attention, funding, career advancement, and doctrinal authority away from total-war contingency planning and toward limited-war and crisis-management frameworks, transferring prestige and resources to the intellectual communities who specialize in the latter and letting general-staff capacity for the former erode through disuse.
% ABSENT_VOICES: Revisionist peer states whose own strategic cultures may not share the Western ideational retirement of total-war thinking are not part of the conversation that produced this discourse drop; officers and planners from an earlier generation who retained total-war doctrinal fluency have retired or been marginalized without their institutional knowledge being formally debriefed or preserved.
% DISAPPEARANCE_RATIONALE: If the ideational shift reversed overnight and total war returned to central elite discourse, defense intellectuals invested in limited-war frameworks would lose relative standing and funding streams would reallocate, but whether the underlying planning capability could be reconstituted quickly is disputed — some argue institutional knowledge and doctrine could be rebuilt within a planning cycle, others argue decades of atrophy in war-gaming infrastructure, officer training, and industrial mobilization planning cannot be reversed merely by a discourse shift, which is precisely why this reading treats the constraint as piton-class (inertial, not merely discursive).
% FOUNDING_PROBLEM: In the early Cold War, total-war planning threatened to consume all strategic bandwidth and crowd out the analytically tractable, more survivable problem of managing crises and limited conflicts without escalation to mutual destruction; the ideational shift toward limited-war frameworks solved the problem of how to think productively about conflict below the threshold of civilizational catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: Historians of strategic thought (e.g., accounts of the RAND Corporation's shift from massive-retaliation to flexible-response literatures) attest the ideational shift was a genuine response to an intractable planning problem in the 1950s-60s. However, more recent peer-competition analysts writing from outside the limited-war intellectual tradition — including some current force-planning critiques — attest that the founding problem (total war being unthinkably unmanageable) has been overtaken by renewed peer rivalry, and that the discursive habit persists past its founding justification, corroborating the piton reading rather than a still-live coordination function.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, contested).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__strategic_culture_drift, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate and rising (0.12 to 0.42 across the interval) because the constraint is not primarily rent-extracting in a direct financial sense — it is an opportunity cost imposed on institutional flexibility, growing as the generational cohort with total-war doctrinal fluency has retired and been replaced by cohorts trained exclusively in limited-war and crisis-management paradigms. Theater ratio is high and rising (0.15 to 0.68) because as actual total-war planning capacity eroded, the performative signaling of 'readiness' (routine doctrine reviews, symbolic war-gaming exercises that no longer stress-test full-scale mobilization assumptions) increasingly substituted for the real function. Suppression is moderate (0.31) — this is not a coercively enforced discourse; no one is punished for raising total-war scenarios, but the professional and institutional incentive gradient makes doing so career-costly and analytically unrewarded, which is a softer form of the same effect. Accessibility collapse (0.4) and resistance (0.35) are both moderate-low, reflecting that the retired framework remains recoverable in principle (unlike a true mountain) but faces real friction from entrenched intellectual and institutional interests.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of limited_war_defense_intellectuals, the shift toward limited-war frameworks looks like hard-won analytical progress — a genuine coordination achievement that made Cold War crisis management possible and prevented reckless escalation thinking. From the seat of strategic_planning_flexibility (the atrophied capacity itself) and peer_conflict_deterrence_credibility, the same shift looks like an inertial loss: institutional forgetting that has left planning bodies unprepared for renewed peer competition and weakened the credibility of full-spectrum deterrence. The engine should compute these seats differently given the same structural data — a piton read from the payer seats, closer to a rope or scaffold read from the beneficiary seats who experienced the original transition as solving a real problem.
 *
 * DIRECTIONALITY LOGIC:
 *   Limited_war_defense_intellectuals and the arms_control_epistemic_community are declared beneficiaries because their institutional standing, funding, and doctrinal authority derive directly from total-war planning remaining marginal to elite discourse — d sits near the beneficiary end for both, with limited_war_defense_intellectuals held even lower via identity-lock (their professional identity is constituted by the limited-war framework, not merely benefited by it). Strategic_planning_flexibility and peer_conflict_deterrence_credibility are declared victims: they bear the cost of the atrophy but have no exit — flexibility cannot 'leave' its own erosion, and deterrence credibility is a diffuse public good degraded by the discourse shift, not a party with a seat at the table capable of resisting it. This is precisely what qualifies the constraint as piton rather than snare: the beneficiaries do not administer an enforcement apparatus extracting from identifiable victims; the costs are diffuse institutional atrophy rather than concentrated capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (total-war planning was combinatorially unmanageable and threatened to crowd out productive strategic thought in the early Cold War) has plausibly been overtaken by renewed peer-competition dynamics that make total-war contingency planning newly relevant, while the discursive habit of treating it as beyond consideration persists. This is the classic piton signature: mandatrophy without a captor. No single actor administers the discourse drop as an enforcement mechanism, and no concentrated beneficiary extracts rents from the erosion the way a snare's operator would. Classifying this as piton rather than tangled_rope prevents mislabeling institutional inertia as active extraction requiring enforcement — the mechanism here is forgetting and habituated attention allocation, not coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indeterminacy_reachability_mechanism,
    'Is total war''s absence from elite discourse better explained by ideational/institutional drift (this reading), physical impossibility from nuclear deterrence (structural_contraction_reading), or normative illegitimacy under international law (normative_reading_drop)? These are not mutually exclusive, but they imply different reversibility profiles and different beneficiary/victim structures.',
    'Comparative institutional analysis: track whether total-war planning capacity would be reconstituted faster in a scenario where (a) nuclear weapons were eliminated, (b) international law norms shifted, or (c) a new generation of strategists deliberately revived total-war doctrine absent either change. Historical natural experiments (e.g., planning behavior in nuclear-armed vs. non-nuclear peer rivalries) could partially disambiguate.',
    'If the structural_contraction_reading is correct, this piton framing is a category error — the capacity isn''t atrophied, it''s foreclosed by physics, and no amount of institutional attention could restore it. If the normative_reading_drop is correct, restoring total-war planning would require not just institutional attention but overturning settled legal doctrine, a much higher bar than this reading assumes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_indeterminacy_reachability_mechanism, conceptual, 'Which of three candidate mechanisms (ideational, structural, normative) actually accounts for total war''s discursive disappearance, and whether they are separable at all.').

omega_variable(
    atrophy_reversibility_uncertainty,
    'Given decades of institutional forgetting, could total-war planning capacity actually be reconstituted on short notice if elite discourse shifted back, or has the atrophy become effectively irreversible (making this closer to a mountain than a piton)?',
    'Examine historical precedents for military doctrine reconstitution after long dormancy (e.g., interwar armor doctrine revival, post-Vietnam conventional warfighting doctrine rebuilding) and assess the time constants involved relative to a plausible crisis timeline.',
    'If reconstitution is fast, the piton classification holds cleanly (recoverable, current form is inertial). If reconstitution is effectively impossible within relevant strategic timeframes, the constraint functions more like an accidental mountain — a constructed limit that has become as binding as a natural one, which would need its own beneficiary-flagged FSM-style treatment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_reversibility_uncertainty, empirical, 'Whether the atrophied total-war planning capacity is genuinely recoverable (piton) or has crossed into practical irreversibility.').

omega_variable(
    defense_intellectual_beneficiary_intentionality,
    'Do limited-war defense intellectuals actively maintain the discourse retirement as self-interested boundary maintenance, or is their beneficiary status an unintended byproduct of a genuine intellectual achievement they do not experience as self-serving?',
    'Elite interview study or archival analysis of internal deliberations within defense-intellectual institutions (e.g., RAND, service war colleges) to assess whether total-war scenarios are excluded from curricula and funding calls through deliberate gatekeeping or through genuine analytical judgment about tractability.',
    'If deliberate gatekeeping is found, the constraint drifts toward tangled_rope (coordination function plus identifiable capture) rather than pure piton (no concentrated beneficiary actively maintaining the arrangement). If genuine judgment, the piton reading is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defense_intellectual_beneficiary_intentionality, empirical, 'Whether beneficiary status among defense intellectuals reflects active self-interested maintenance or an unintended structural byproduct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(tota_tr_t1962, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1962, 0.25).
narrative_ontology:measurement(tota_tr_t1979, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1979, 0.4).
narrative_ontology:measurement(tota_tr_t1991, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1991, 0.55).
narrative_ontology:measurement(tota_tr_t2008, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2008, 0.62).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2025, 0.68).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1945, 0.12).
narrative_ontology:measurement(tota_be_t1962, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1962, 0.18).
narrative_ontology:measurement(tota_be_t1979, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1979, 0.25).
narrative_ontology:measurement(tota_be_t1991, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1991, 0.31).
narrative_ontology:measurement(tota_be_t2008, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2008, 0.37).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2025, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_winnability_post1945__strategic_culture_drift, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__strategic_culture_drift, identity_coordination).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__strategic_culture_drift, 0.1).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, normative_reading_drop).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the total_war_winnability_post1945 kernel, each authored as a separate ε-invariant story per the decomposition principle. structural_contraction_reading holds nuclear weapons physically removed total war from the reachable space (near-mountain, minimal extraction, no ideational beneficiary). normative_reading_drop holds international humanitarian law delegitimized total war (a legal/normative constraint with different beneficiary structure — international law professionals and treaty bodies rather than defense intellectuals). This reading (strategic_culture_drift) holds the change is ideational/institutional and classifies as piton, with limited-war defense intellectuals as the concentrated beneficiary and strategic flexibility as the diffuse victim. All three observe the same surface phenomenon but authored independently with different ε, different beneficiary/victim sets, and different type classifications, linked via affects_constraints per DP-001.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
