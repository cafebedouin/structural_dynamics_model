% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: Nuclear Winnability Rhetorical Taboo (Post-1945)
 *   domain: strategic/nuclear/political
 *
 * SUMMARY:
 *   After 1945, nuclear winnability underwent a dual-layer contraction. In
 *   public discourse, the concept became rhetorically unsayable — a taboo
 *   enforced by scientific consensus, moral revulsion, and the logic of
 *   mutual assured destruction. 'No one can win a nuclear war' became the
 *   required catechism. Simultaneously, inside classified planning,
 *   winnability remained an operational category: counterforce targeting,
 *   damage limitation, escalation control, and 'prevailing' in a nuclear
 *   exchange were planned, exercised, and resourced. The taboo performs a
 *   genuine coordination function (stabilizing deterrence by foreclosing
 *   victory talk) while extracting democratic accountability from the
 *   planning apparatus. Strategic planners gain operational flexibility
 *   without public scrutiny; democratic oversight loses the ability to
 *   contest doctrinal choices hidden behind the taboo.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.72).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.78).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.72).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "Nuclear Winnability Rhetorical Taboo (Post-1945)").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic/nuclear/political").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, 'd76b421e-3419-4977-803a-c3c200045e7e').
narrative_ontology:cs_kernel_codification('d76b421e-3419-4977-803a-c3c200045e7e', implicit).
narrative_ontology:cs_authority_grounding('d76b421e-3419-4977-803a-c3c200045e7e', practice).
narrative_ontology:cs_interpretation_layer_present('d76b421e-3419-4977-803a-c3c200045e7e').
narrative_ontology:cs_reading_relation('d76b421e-3419-4977-803a-c3c200045e7e', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('d76b421e-3419-4977-803a-c3c200045e7e', war_winnability_post_1945__countervailing_thinkable, influences).
narrative_ontology:cs_axiom('d76b421e-3419-4977-803a-c3c200045e7e', foundational, winnability_rhetorically_foreclosed).
narrative_ontology:cs_axiom_status(winnability_rhetorically_foreclosed, holdable).
narrative_ontology:cs_axiom_grounding('d76b421e-3419-4977-803a-c3c200045e7e', winnability_rhetorically_foreclosed, conventional).
narrative_ontology:cs_axiom('d76b421e-3419-4977-803a-c3c200045e7e', foundational, planning_operational_continuity).
narrative_ontology:cs_axiom_status(planning_operational_continuity, holdable).
narrative_ontology:cs_axiom_grounding('d76b421e-3419-4977-803a-c3c200045e7e', planning_operational_continuity, conventional).
narrative_ontology:cs_reference_frame('d76b421e-3419-4977-803a-c3c200045e7e', mutual_deterrence_stability).
narrative_ontology:cs_drift_state('d76b421e-3419-4977-803a-c3c200045e7e', post_cold_war_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d76b421e-3419-4977-803a-c3c200045e7e', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, nuclear_establishment).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, democratic_oversight).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, public_discourse).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, arms_control_community).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, deterrence_stability_norm).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, nuclear_taboo).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, mutual_assured_destruction_credibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain classified targeting doctrines, war plans, and escalation ladders that presuppose controllable nuclear use. The rhetorical taboo shields their planning from public scrutiny and congressional oversight while preserving operational flexibility. They author the classified guidance that defines what 'winnable' means operationally, while publicly endorsing the taboo.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_planners, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, strategic_planners, beneficiary).

% Complex of labs, commands, contractors, and allied bureaucracies that receive funding and authority from the perpetuation of nuclear planning missions. The taboo protects their institutional turf — questioning winnability threatens force structure, modernization programs, and career paths. They can rotate between government, industry, and think tanks, preserving influence across administrations.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, nuclear_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% Congress, GAO, inspectors general, and elected officials nominally responsible for authorizing and overseeing nuclear policy. They bear the cost of not knowing what planners actually plan: authorization votes become performative, budgets fund capabilities whose doctrine is classified, and the constitutional war power atrophies. Exit from ignorance requires security clearances and institutional will that rarely materializes.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, democratic_oversight, payer,
    organized, generational, constrained, national).

% Academic strategists, journalists, NGOs, and informed citizens who operate in the unclassified domain. They pay the epistemic cost of a discourse that treats winnability as conceptually incoherent while planning assumes it is achievable. Their analyses cannot engage the real operational logic; they debate a straw man. Exit requires clearance access that transforms them into insiders, losing public voice.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, public_discourse, payer,
    organized, biographical, constrained, global).

% Treaty negotiators, verification specialists, and disarmament advocates who would challenge the taboo's operational hypocrisy. They are structurally excluded from classified planning circles; their models assume declaratory policy matches operational reality. When they expose gaps (e.g., counterforce capabilities vs. assured destruction rhetoric), they are dismissed as naive or destabilizing.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, arms_control_community, excluded,
    organized, biographical, constrained, global).

% Sees the dual-layer structure: the public taboo that stabilizes deterrence by making nuclear use 'unthinkable,' and the classified planning that treats nuclear use as a constrained operational problem. The observer notes the taboo performs a genuine coordination function (deterrence stability) while simultaneously extracting democratic accountability from planning choices.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rhetorical taboo coordinates great-power behavior by establishing a shared normative boundary: nuclear war is not a usable instrument of policy. This stabilizes mutual deterrence by removing victory as a coherent category in public discourse, reducing pressure for preemptive escalation during crises.
% TRANSFER_FUNCTION: Transfers operational planning freedom and doctrinal authority from democratic accountability structures (Congress, public debate, legal review) to the classified strategic planning apparatus. The taboo acts as a one-way membrane: planners see the operational reality; overseers see only the rhetorical performance.
% ABSENT_VOICES: Populations that would be targets of planned nuclear strikes (no voice in planning), whistleblowers who face prosecution for revealing operational doctrines (e.g., Ellsberg, Snowden-adjacent), Global South states excluded from nuclear decision-making but bearing fallout risks, and critical strategists denied clearance who cannot falsify or confirm planning assumptions.
% DISAPPEARANCE_RATIONALE: If the rhetorical taboo vanished overnight — planners publicly acknowledged winnability as a planned-for contingency — democratic oversight would demand doctrinal review, Congress would require hearings on targeting criteria, arms control negotiations would shift from numerical limits to doctrinal constraints, and the declaratory/operational gap would become a political crisis. The planning apparatus would lose its autonomy.
% FOUNDING_PROBLEM: How to maintain credible deterrence against a nuclear-armed adversary without legitimizing nuclear war-fighting as a rational strategic option. The taboo emerged from the 1950s-60s recognition that public discussion of 'winning' a nuclear war undermined the mutual vulnerability that made deterrence stable.
% FOUNDING_PROBLEM_CORROBORATION: McNamara's 'no cities' speech (1962) and Schelling's 'reciprocal fear of surprise attack' attest the founding problem was genuine. Current strategic planners (per 2018 NPR, 2022 NPR) attest the problem persists but now cite 'tailored deterrence' requiring flexible options. Arms Control Association, Physicians for Social Responsibility, and former STRATCOM commanders (e.g., Gen. Cartwright) attest the operational doctrine has expanded beyond the founding problem's scope while the taboo prevents democratic adjudication of that expansion.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the planning apparatus captures the full value of nuclear modernization and doctrinal innovation while externalizing the political cost to the taboo. Suppression (0.78) is higher because the taboo's persistence depends on active enforcement: classification, security clearance regimes, the 'born classified' doctrine (Atomic Energy Act), and professional sanctions for violating the normative boundary. Theater ratio (0.48) is near parity — the public performance of 'unthinkability' is now almost as elaborate as the planning it screens. Accessibility collapse (0.82) is high because the unclassified literature structurally cannot engage the classified planning logic; alternatives to the taboo (e.g., open doctrinal debate) are collapsed by the clearance barrier. Resistance (0.42) is moderate — arms control advocates, whistleblowers, and critical academics resist but lack structural leverage.
 *
 * PERSPECTIVAL GAP:
 *   From the planner's seat, the taboo is a necessary coordination device that prevents adversary miscalculation; the classified planning is prudent hedging. From the democratic oversight seat, the same structure is an accountability vacuum: they authorize budgets for doctrines they cannot see. From the public discourse seat, the taboo creates an epistemic trap — analyzing 'deterrence' while the real object is 'war-fighting.' The engine computes this divergence from the declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic planners and the nuclear establishment are structural beneficiaries (d near 0.0): they collect planning autonomy, budget authority, and institutional survival from the taboo. Democratic oversight, public discourse, and arms control community are targets (d near 1.0): they bear the epistemic and accountability costs. The analytical observer sits at d=0.5 (symmetric). The derivation chain correctly places planners at the beneficiary end because they control the classification system that defines what is sayable; democratic oversight is identity-locked to its constitutional role (cannot exit oversight) but constrained by clearance barriers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stabilizing deterrence) remains live, but the taboo has drifted from solving it to shielding an expanded planning mission. The coordination function (deterrence stability) is real but now coexists with an extraction function (planning autonomy without oversight). This is the tangled_rope signature: genuine coordination + asymmetric extraction + active enforcement. The mandatrophy is not resolved because the coordination function still operates; the taboo would not persist if it were pure extraction. But the extraction layer has thickened over 80 years — the taboo now protects counterforce capabilities and flexible response options that the founding generation explicitly rejected as destabilizing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_genuine_vs_instrumental,
    'Is the rhetorical taboo genuinely believed by its enforcers as a stabilizing norm, or is it instrumentally maintained as cover for planning expansion?',
    'Internal memoirs, declassified planning guidance vs. public statements correlation; whether planners privately acknowledge winnability as a planning category while publicly denying it.',
    'If instrumental, the constraint is a snare (coordination story is pure cover). If genuine but captured, it remains tangled_rope. The engine''s classification hinges on whether the coordination function has independent persistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_genuine_vs_instrumental, conceptual, 'Whether the taboo''s coordination function is sincere or performed.').

omega_variable(
    planning_belief_vs_optionality,
    'Do strategic planners actually believe limited nuclear victory is achievable, or do they maintain the optionality as bureaucratic insurance without conviction?',
    'Oral histories, wargame transcripts, and doctrinal evolution: if plans shift from ''prevailing'' to ''controlling escalation'' without changing force posture, optionality dominates belief.',
    'If planners don''t believe winnability is real, the operational layer is theater — the constraint becomes piton (atrophied coordination, theatrical maintenance). If they believe it, the extraction is substantive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(planning_belief_vs_optionality, empirical, 'Whether operational planning reflects genuine conviction or institutional inertia.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of winnability discourse structural (classification, clearance barriers) or internalized (strategic community self-censors because the taboo is identity-constitutive)?',
    'Post-Cold War declassification: when structural barriers lowered (e.g., 1990s openness), did discourse expand or did practitioners self-limit? Compare nuclear vs. conventional doctrinal openness.',
    'If internalized, the constraint''s effective suppression exceeds structural measures — the community carries the taboo as professional identity. This would raise effective extraction for the analytical seat (observers internalize the boundary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the strategic community.').

omega_variable(
    committer_frame_location,
    'This constraint is one reading (rhetorical_contraction) of the contested kernel ''war_winnability_post_1945''. Where exactly do the sibling readings differ structurally?',
    'Map each reading''s beneficiary/victim structure and claimed_type. deterrence_unthinkable: no operational layer, Mountain claim. countervailing_thinkable: no taboo layer, Rope claim. rhetorical_contraction: both layers, Tangled Rope claim.',
    'Clarifies that the three readings are distinct constraints with different ε values, not one constraint viewed from three angles. Each instantiates a different structural claim about the post-1945 arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_location, conceptual, 'Kernel reading decomposition: three constraints, not one with three perspectives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war_winnability_rhetorical_contraction_tr_t1945, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_tr_t1955, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1955, 0.15).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_tr_t1965, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1965, 0.22).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_tr_t1975, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_tr_t1985, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1985, 0.38).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_tr_t1995, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1995, 0.42).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_tr_t2005, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2005, 0.45).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_tr_t2015, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2015, 0.47).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_tr_t2025, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(war_winnability_rhetorical_contraction_be_t1945, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_be_t1955, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1955, 0.25).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_be_t1965, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_be_t1975, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_be_t1985, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_be_t1995, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_be_t2005, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_be_t2015, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_be_t2025, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(war_winnability_rhetorical_contraction_su_t1945, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1945, 0.3).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_su_t1955, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1955, 0.45).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_su_t1965, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_su_t1975, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_su_t1985, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1985, 0.68).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_su_t1995, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1995, 0.72).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_su_t2005, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_su_t2015, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement(war_winnability_rhetorical_contraction_su_t2025, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__rhetorical_contraction, 0.1).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__countervailing_thinkable).

% DUAL FORMULATION NOTE:
% Kernel 'war_winnability_post_1945' decomposes into three constraint stories with different ε and structural profiles. This reading (rhetorical_contraction) identifies the dual-layer structure as the constraint itself. deterrence_unthinkable reading claims the operational layer is epiphenomenal (Mountain). countervailing_thinkable reading claims the rhetorical layer is mistaken (Rope). The three stories form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__rhetorical_contraction, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
