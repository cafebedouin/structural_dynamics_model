% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__deterrence_equilibrium_reading, []).

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
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Nuclear Deterrence Equilibrium — Mutual Vulnerability as Cost-Benefit Constraint
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This constraint story captures the deterrence equilibrium reading of the
 *   total war possibility space kernel. The claim: nuclear weapons created
 *   mutual vulnerability that makes total war irrational for rational actors,
 *   so war remains in the planning space but is deterred by cost-benefit
 *   calculation. This generates continuous investment in war-fighting
 *   capabilities (counterforce targeting, escalation ladders, modernization)
 *   as deterrent signals. The constraint is a tangled rope: it genuinely
 *   coordinates by preventing great-power war (coordination function), but
 *   extracts massively through the permanent war-fighting posture it requires
 *   (extraction function), and requires active enforcement (alert postures,
 *   exercises, modernization programs, non-proliferation regime). The claimed
 *   type (tangled_rope) and metrics are authored independently — the engine
 *   will compute per-seat classifications from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.72).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.45).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Nuclear Deterrence Equilibrium — Mutual Vulnerability as Cost-Benefit Constraint").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, '69cea7bb-5d71-4390-9faa-e1413745b55a').
narrative_ontology:cs_kernel_codification('69cea7bb-5d71-4390-9faa-e1413745b55a', distributed).
narrative_ontology:cs_authority_grounding('69cea7bb-5d71-4390-9faa-e1413745b55a', practice).
narrative_ontology:cs_interpretation_layer_present('69cea7bb-5d71-4390-9faa-e1413745b55a').
narrative_ontology:cs_reading_relation('69cea7bb-5d71-4390-9faa-e1413745b55a', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('69cea7bb-5d71-4390-9faa-e1413745b55a', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('69cea7bb-5d71-4390-9faa-e1413745b55a', foundational, mutual_vulnerability_deters_through_rational_calculation).
narrative_ontology:cs_axiom_status(mutual_vulnerability_deters_through_rational_calculation, holdable).
narrative_ontology:cs_axiom_grounding('69cea7bb-5d71-4390-9faa-e1413745b55a', mutual_vulnerability_deters_through_rational_calculation, empirically_contingent).
narrative_ontology:cs_axiom('69cea7bb-5d71-4390-9faa-e1413745b55a', secondary, war_fighting_capability_signals_resolve).
narrative_ontology:cs_axiom_status(war_fighting_capability_signals_resolve, holdable).
narrative_ontology:cs_axiom_grounding('69cea7bb-5d71-4390-9faa-e1413745b55a', war_fighting_capability_signals_resolve, instrumental).
narrative_ontology:cs_reference_frame('69cea7bb-5d71-4390-9faa-e1413745b55a', mutual_vulnerability_stability).
narrative_ontology:cs_drift_state('69cea7bb-5d71-4390-9faa-e1413745b55a', post_cold_war_multipolar_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('69cea7bb-5d71-4390-9faa-e1413745b55a', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_establishments).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, military_industrial_complexes).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_armed_states_leadership).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, global_populations_nuclear_risk).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, future_generations_nuclear_legacy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_armed_states_leadership).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, rational_deterrence_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, mutual_assured_destruction_stability).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_revolution_in_military_affairs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nuclear weapons laboratories, command structures, and doctrinal communities that plan, maintain, and justify nuclear arsenals. Their organizational identity, budgets, and professional status are fused to the deterrence mission. They define what counts as credible deterrence and continuously expand requirements (modernization, new capabilities, lower-yield options). Exit would mean institutional dissolution or radical repurposing — neither is organizationally thinkable.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_establishments, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_establishments, beneficiary).

% Defense contractors and associated political-economic networks that receive sustained, multi-decade funding for nuclear modernization programs (warheads, delivery systems, command/control, infrastructure). They lobby for threat assessments that justify new programs and capture regulatory oversight. Exit is easy — they can pivot to conventional or other sectors — but the nuclear mission is their most reliable revenue stream.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, military_industrial_complexes, beneficiary,
    powerful, biographical, arbitrage, global).

% Political leaders of the nine nuclear-armed states who authorize postures, budgets, and declaratory policies. They benefit from the status and strategic autonomy nuclear weapons confer, but bear the political risk of accidents, crises, and opportunity costs (resources not spent on domestic needs). Exit (disarmament) is constrained by alliance commitments, domestic nuclear constituencies, and fear of cheating/breakout.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_armed_states_leadership, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_armed_states_leadership, payer).

% The world's population living under the permanent risk of nuclear war — deliberate, accidental, or unauthorized. They bear the existential risk and the opportunity costs of nuclear spending without consent or meaningful influence. Exit is structurally impossible: no state offers citizenship without nuclear risk, and individual mitigation (bunkers, relocation) is illusory against global effects.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, global_populations_nuclear_risk, payer,
    powerless, generational, trapped, global).

% The 180+ NNWS parties to the NPT who forego nuclear weapons in exchange for disarmament commitments (Article VI) and peaceful-use cooperation. They bear the risk of nuclear war without the purported security benefits, and watch disarmament obligations go unfulfilled. Exit options: acquire nuclear weapons (proliferation, high cost, sanctions), join TPNW (symbolic, no material change), or remain in NPT bargain (status quo).
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_weapon_states, excluded).

% Generations not yet born who will inherit the radiological legacy (waste, contamination), the risk of deterrence failure, and the path-dependence of nuclear infrastructure. They have no voice in current decisions and no exit from the world they will inherit.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, future_generations_nuclear_legacy, payer,
    powerless, civilizational, trapped, global).

% Diplomats, verification experts, NGOs, and scholars who negotiate treaties, monitor compliance, and produce knowledge about nuclear risks. They operate within the deterrence framework (accepting its premises) while seeking to reduce its dangers. Their influence is real but bounded by the agenda-setters' willingness to concede.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, arms_control_community, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents total war among nuclear-armed states by making the cost of any nuclear exchange unacceptably high for all parties — a genuine coordination solution to the security dilemma that would otherwise drive major-power conflict.
% TRANSFER_FUNCTION: Moves vast material resources (trillions of dollars over decades), scientific/engineering talent, and political capital from societal needs (health, education, infrastructure, climate) into nuclear weapons complexes and their sustaining institutions. Moves existential risk from leaders' calculations onto populations who have no say. Moves strategic autonomy to nuclear-armed states at the expense of non-nuclear states' security.
% ABSENT_VOICES: Future generations (structurally excluded from any political process); populations of nuclear-armed states (never consulted on deterrence posture, only mobilized during crises); Global South states (bear disproportionate climate/nuclear winter effects from Northern deterrence postures). These voices are absent because the constraint's enforcement mechanism (nuclear command authority) concentrates decision power in a handful of individuals.
% DISAPPEARANCE_RATIONALE: If mutual vulnerability deterrence vanished overnight, nuclear-armed states would immediately face a transformed strategic landscape: either rapid disarmament (if the taboo/space-contraction readings hold) or uncontrolled re-armament and probable use (if deterrence was the only barrier). The global nuclear order, alliance structures, and trillion-dollar modernization programs would all reorganize.
% FOUNDING_PROBLEM: The founding problem was the apparent inevitability of great-power total war in the industrial age — WWI and WWII demonstrated that conventional deterrence fails catastrophically. Nuclear weapons were developed and deployed to make the cost of great-power war so high that no rational leader would initiate it, solving the security dilemma through mutual vulnerability rather than defense.
% FOUNDING_PROBLEM_CORROBORATION: The nuclear establishments and deterrence theorists attest the problem is live: great-power conflict remains possible and mutual vulnerability remains the only reliable barrier. Critics (TPNW states, disarmament NGOs, some former officials like Perry, Shultz, Kissinger, Nunn) attest the problem is transformed: the Cold War security dilemma is gone, but the deterrence machinery persists and generates new risks. The 2017 Nobel Peace Prize to ICAN and the TPNW's entry into force are external corroboration that the founding problem's status is genuinely disputed.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint diverts civilizational-scale resources into systems whose only justified use is never using them — a permanent mobilization on hair-trigger alert. Suppression (0.45) is moderate: the constraint doesn't suppress alternatives through direct coercion of populations, but through structural lock-in (NPT bargain, alliance commitments, identity-locked establishments). Theater ratio (0.38) reflects that a substantial fraction of nuclear activity (parades, rhetorical signaling, capabilities beyond survivable second strike) is performative deterrence signaling rather than functional coordination. Accessibility collapse (0.55) is intermediate: alternatives (disarmament, taboo, space contraction) are thinkable and advocated but structurally blocked by the constraint's own enforcement machinery. Resistance (0.35) is present but fragmented — TPNW, divestment campaigns, whistleblowers — insufficient to shift the equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   The nuclear establishment seat experiences this as a genuine coordination success (no great-power war since 1945) with manageable costs. The trapped payer seats (global populations, future generations) experience it as an extractive snare imposing existential risk without consent. The NNWS seat experiences it as a broken bargain (NPT Article VI unfulfilled). The engine computes these divergences from the structural data — the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear establishments are identity-locked agenda-setters who also benefit (d ~ 0.15 — they set the rules and their survival depends on the constraint). Military-industrial complexes are arbitrage-grade beneficiaries (d ~ 0.10 — they capture rents with easy exit). Nuclear-armed leadership are constrained agenda-setters/payers (d ~ 0.55 — they bear political risk and opportunity cost but control the posture). Global populations are trapped payers (d ~ 0.95 — existential risk, zero exit, zero voice). NNWS are constrained payers/excluded (d ~ 0.75 — bear risk without benefit, limited exit). Future generations are trapped payers (d ~ 1.0 — maximal extraction, zero agency). Arms control community are analytical observers (d ~ 0.50 — symmetric costs/benefits within the framework).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing industrial great-power war) was genuinely solved by mutual vulnerability. But the mandate has hypertrophied: the deterrence logic now requires capabilities far beyond assured retaliation (counterforce, damage limitation, prompt launch, nuclear modernization without end). The coordination function (war prevention) is real but the extraction function (permanent war-fighting complex) has grown disproportionate. The constraint persists not because the founding problem requires this level of extraction, but because the establishments that administer it are identity-locked and the extraction beneficiaries (MIC) have arbitrage-grade influence. This is mandatrophy: a solved problem's solution has become a self-justifying institution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_vs_taboo_causality,
    'Is the non-use of nuclear weapons since 1945 caused by rational deterrence calculation (mutual vulnerability), by normative taboo, or by strategic unthinkability — and how much does each contribute?',
    'Counterfactual analysis of near-use incidents (Cuban Missile Crisis, Able Archer, 1983 Soviet false alarm, Kargil, etc.): did leaders explicitly calculate costs/benefits, invoke taboo, or simply not consider use? Declassified records and oral histories from multiple nuclear-armed states.',
    'If taboo or unthinkability does the heavy lifting, the deterrence equilibrium''s extractive apparatus (counterforce, modernization, escalation ladders) is largely unnecessary for its stated coordination function — strengthening the tangled_rope classification. If deterrence calculation is primary, the extraction is the price of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_vs_taboo_causality, empirical, 'Causal attribution for nuclear non-use since 1945 — deterrence vs taboo vs unthinkability.').

omega_variable(
    counterforce_necessity,
    'Are counterforce targeting, damage-limitation capabilities, and escalation ladders functionally necessary for deterrence stability, or are they extractive expansions by nuclear establishments?',
    'Comparative analysis of deterrence stability in minimal-deterrence postures (China''s historical posture, UK/France) vs. counterforce-heavy postures (US/Russia). War-gaming and historical crisis analysis: did counterforce capabilities ever change an adversary''s calculation in a crisis?',
    'If counterforce is unnecessary for deterrence, the measured extractiveness (0.72) is substantially inflated by establishment-driven capability growth — the constraint is more extractive than its coordination function requires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterforce_necessity, empirical, 'Whether counterforce/war-fighting capabilities are deterrence necessities or extractive expansions.').

omega_variable(
    committer_frame_underdetermination,
    'Does the deterrence_equilibrium_reading foreclose the space_contraction_reading, or do they occupy compatible but distinct analytical frames?',
    'Examine whether any single strategic framework can simultaneously hold ''war is thinkable but deterred'' and ''war is unthinkable'' — or whether these are descriptions of different cognitive layers (explicit planning vs. implicit assumption) that can coexist in one mind/institution.',
    'If they foreclose, the kernel has a genuine logical fracture and the readings are rival paradigms. If they coexist, the kernel''s contest is about emphasis and policy implication, not logical contradiction — affecting how the engine models reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_underdetermination, conceptual, 'Logical relationship between deterrence equilibrium and space contraction readings of the total war possibility space kernel.').

omega_variable(
    suppression_mechanism_in_deterrence,
    'Is the constraint''s suppression of alternatives (disarmament, taboo internalization, space contraction) structural (institutional lock-in, alliance commitments) or internalized (deterrence logic becomes the only thinkable framework for security elites)?',
    'Track post-Cold War disarmament initiatives (Canberra Commission, Tokyo Forum, Nuclear Zero movements, TPNW): when structural barriers lowered (end of USSR), did internalized deterrence thinking prevent uptake? Compare elite discourse in nuclear vs non-nuclear states.',
    'If internalized suppression dominates, the constraint''s effective suppression is higher than institutional measures suggest — the constraint reproduces itself through the cognitive categories of its administrators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_in_deterrence, empirical, 'Structural vs internalized suppression of disarmament alternatives in the nuclear order.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twps_der_tr_t1945, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(twps_der_tr_t1960, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(twps_der_tr_t1975, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1975, 0.35).
narrative_ontology:measurement(twps_der_tr_t1990, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(twps_der_tr_t2005, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(twps_der_tr_t2025, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(twps_der_be_t1945, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(twps_der_be_t1960, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(twps_der_be_t1975, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1975, 0.65).
narrative_ontology:measurement(twps_der_be_t1990, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(twps_der_be_t2005, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(twps_der_be_t2025, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(twps_der_su_t1945, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(twps_der_su_t1960, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(twps_der_su_t1975, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(twps_der_su_t1990, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(twps_der_su_t2005, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(twps_der_su_t2025, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__deterrence_equilibrium_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_nonproliferation_regime).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_alliances).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_modernization_programs).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the total_war_possibility_space kernel. The deterrence_equilibrium_reading (this story) treats mutual vulnerability as a rational cost-benefit constraint generating continuous war-fighting investment. The space_contraction_reading treats nuclear weapons as removing total war from the strategically thinkable. The nuclear_taboo_reading treats non-use as a constructed normative prohibition. All three share the referent (nuclear non-use since 1945) but disagree on mechanism, implying different ε values and different policy prescriptions. They form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__deterrence_equilibrium_reading, institutional, 0.2).
constraint_indexing:directionality_override(total_war_possibility_space__deterrence_equilibrium_reading, powerful, 0.15).
constraint_indexing:directionality_override(total_war_possibility_space__deterrence_equilibrium_reading, moderate, 0.7).
constraint_indexing:directionality_override(total_war_possibility_space__deterrence_equilibrium_reading, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
