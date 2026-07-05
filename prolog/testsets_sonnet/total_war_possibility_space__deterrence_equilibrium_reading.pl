% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Deterrence-Equilibrium Reading of the Total-War Possibility Space
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This story instantiates the deterrence-equilibrium reading of the
 *   total-war possibility-space kernel: total war remains a live, calculable
 *   option in the strategic planning space, held out of actual occurrence
 *   only by the cost-benefit arithmetic of mutual vulnerability. Under this
 *   reading, the constraint's mechanism is continuous investment in
 *   war-fighting capability as a legible deterrent signal — doctrine
 *   development, counterforce targeting, and escalation-ladder theorization
 *   are not residue but the load-bearing structure that keeps the equilibrium
 *   credible. This is a distinct constraint from the sibling readings: the
 *   nuclear-taboo reading locates the restraint in constructed normative
 *   prohibition independent of capability, and the space-contraction reading
 *   holds that total war has been removed from the thinkable, not merely the
 *   preferable. Each reading has a different epsilon and a different
 *   beneficiary structure; they are linked here only through
 *   network.affects_constraints and the kernel-context note, not merged.
 *
 * KEY AGENTS:
 *   - nuclear_weapons_states: agenda_setter/beneficiary (institutional/arbitrage) — sets and maintains doctrine as coordination-plus-extraction
 *   - defense_industrial_base: beneficiary (organized/arbitrage) — captures the procurement stream the reading generates
 *   - strategic_studies_establishment: beneficiary/agenda_setter (institutional/mobile) — professional relevance riding on this reading being operative
 *   - non_nuclear_states: payer (moderate/constrained) — bear asymmetric risk from a possibility space they did not design
 *   - domestic_taxpayers_of_nuclear_states: payer (powerless/trapped) — fund the modernization cycle
 *   - populations_in_client_proxy_conflicts: payer (powerless/trapped) — absorb violence displaced downward by top-level deterrence
 *   - arms_control_analysts: observer (analytical) — assess whether the framing is empirically load-bearing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.61).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.55).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Deterrence-Equilibrium Reading of the Total-War Possibility Space").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, '6c605faf-f9a0-4ae1-b0f5-45e3ba55ffff').
narrative_ontology:cs_kernel_codification('6c605faf-f9a0-4ae1-b0f5-45e3ba55ffff', distributed).
narrative_ontology:cs_authority_grounding('6c605faf-f9a0-4ae1-b0f5-45e3ba55ffff', practice).
narrative_ontology:cs_interpretation_layer_present('6c605faf-f9a0-4ae1-b0f5-45e3ba55ffff').
narrative_ontology:cs_reading_relation('6c605faf-f9a0-4ae1-b0f5-45e3ba55ffff', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c605faf-f9a0-4ae1-b0f5-45e3ba55ffff', total_war_possibility_space__nuclear_taboo_reading, influences).
narrative_ontology:cs_axiom('6c605faf-f9a0-4ae1-b0f5-45e3ba55ffff', foundational, war_remains_calculable_option_under_cost_benefit_analysis).
narrative_ontology:cs_axiom_status(war_remains_calculable_option_under_cost_benefit_analysis, holdable).
narrative_ontology:cs_axiom_grounding('6c605faf-f9a0-4ae1-b0f5-45e3ba55ffff', war_remains_calculable_option_under_cost_benefit_analysis, empirically_contingent).
narrative_ontology:cs_axiom('6c605faf-f9a0-4ae1-b0f5-45e3ba55ffff', secondary, credible_capability_signaling_is_necessary_for_restraint).
narrative_ontology:cs_axiom_status(credible_capability_signaling_is_necessary_for_restraint, holdable).
narrative_ontology:cs_axiom_grounding('6c605faf-f9a0-4ae1-b0f5-45e3ba55ffff', credible_capability_signaling_is_necessary_for_restraint, instrumental).
narrative_ontology:cs_reference_frame('6c605faf-f9a0-4ae1-b0f5-45e3ba55ffff', cold_war_bipolar_deterrence_stability).
narrative_ontology:cs_drift_state('6c605faf-f9a0-4ae1-b0f5-45e3ba55ffff', post_proliferation_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6c605faf-f9a0-4ae1-b0f5-45e3ba55ffff', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_base).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, strategic_studies_establishment).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, domestic_taxpayers_of_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, populations_in_client_proxy_conflicts).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, rational_deterrence_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, mutual_assured_destruction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains, modernizes, and doctrinally justifies continuous investment in counterforce capability, escalation-ladder theorization, and survivable second-strike infrastructure. Frames this as necessary cost-benefit calculus that keeps total war reachable-but-irrational. Sets the terms of what counts as credible deterrence and controls the doctrine-development apparatus. Retains freedom of maneuver that non-nuclear states lack.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapons_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapons_states, beneficiary).

% Receives sustained procurement contracts for delivery systems, warhead modernization, missile defense, and command-control infrastructure justified by the deterrence-equilibrium framing. The continuous investment the reading predicts is this actor's revenue stream; a taboo-based or space-contraction reading of the same kernel would not generate the same procurement logic.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_base, beneficiary,
    organized, generational, arbitrage, national).

% Think tanks, war colleges, and academic strategists build careers theorizing escalation ladders, counterforce targeting doctrine, and crisis-stability models premised on total war remaining a live, calculable option. Their professional relevance depends on the deterrence-equilibrium reading being the operative one rather than the taboo or space-contraction reading.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, strategic_studies_establishment, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, strategic_studies_establishment, agenda_setter).

% Live inside a possibility space they did not design and cannot exit: total war remains strategically reachable for the powers that hold it in reserve, which shapes alliance dependency, extended-deterrence bargains, and vulnerability to great-power crisis dynamics they cannot influence. Cannot credibly threaten total war themselves, so bear the risk asymmetrically.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states, payer,
    moderate, generational, constrained, global).

% Fund the continuous modernization cycle the deterrence-equilibrium reading demands as evidence of credible resolve. Have no direct input into doctrine and cannot opt out of the fiscal transfer; the cost is diffuse, recurring, and justified by a strategic logic they are not positioned to independently verify.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, domestic_taxpayers_of_nuclear_states, payer,
    powerless, biographical, trapped, national).

% Absorb the displaced violence of a system in which total war between principals is deterred but sub-threshold and proxy conflict is not — the equilibrium reading's own logic predicts that competition migrates downward into contests below the nuclear threshold, and these populations bear that migrated cost directly.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, populations_in_client_proxy_conflicts, payer,
    powerless, immediate, trapped, regional).

% Study whether the deterrence-equilibrium framing is empirically load-bearing or self-serving justification for continued weapons investment. Can produce evidence relevant to the omega questions below but do not control doctrine or procurement.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, arms_control_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__deterrence_equilibrium_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_possibility_space__deterrence_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, mutually legible framework — credible retaliatory capability plus rational cost-benefit calculation — that lets rival nuclear powers avoid total war despite persistent strategic competition, by making the costs of escalation to that threshold visibly and continuously prohibitive to both sides.
% TRANSFER_FUNCTION: Moves enormous, recurring fiscal resources from domestic taxpayers of nuclear states toward weapons modernization and doctrine infrastructure; moves risk and displaced violence from the nuclear-armed principals onto non-nuclear states and populations caught in proxy conflicts generated by competition that cannot express itself at the top of the ladder.
% ABSENT_VOICES: Populations in proxy-conflict zones and non-nuclear states bear the downstream costs of the equilibrium but have no seat in doctrine formation, arms-control negotiation, or targeting policy; they would object that the 'equilibrium' externalizes violence onto them rather than eliminating it.
% DISAPPEARANCE_RATIONALE: If the deterrence-equilibrium framing were abandoned in favor of the taboo or space-contraction reading, procurement logic, doctrine schools, and career structures built on continuous war-fighting-capability investment would lose their justification — the defense-industrial and strategic-studies beneficiaries dispute that the world would be safer, while critics argue the underlying restraint would persist on normative or structural grounds regardless of which reading is official.
% FOUNDING_PROBLEM: Rival nuclear powers needed a stable way to avoid total war despite continuing geopolitical competition and the technical reality that first-strike advantages could otherwise incentivize preemption.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear weapons states and strategic-studies institutions attest the problem remains live (citing new nuclear entrants, multipolar competition, hypersonic delivery systems). Arms control analysts and historians outside the beneficiary set corroborate that mutual vulnerability plus normative taboo, not continuous doctrine elaboration, does the actual restraining work — suggesting the founding problem is substantially solved and the doctrine apparatus persists partly as institutional and industrial self-perpetuation.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, contested).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.61) reflects that the deterrence-equilibrium reading, as authored, sustains a large recurring fiscal and geopolitical-risk transfer from taxpayers and non-nuclear/proxy populations toward nuclear-armed principals and their industrial/academic beneficiaries — a transfer the reading itself predicts (continuous investment as deterrent signal) rather than incidentally produces. Suppression (0.55) is moderate: the reading requires active doctrinal and alliance-management enforcement (extended deterrence commitments, non-proliferation regimes) to keep the equilibrium legible, but does not require the near-total alternative-foreclosure a mountain claim would need. Theater ratio (0.42) captures that a substantial share of escalation-ladder theorization and doctrine elaboration is defensible as genuine crisis-stability modeling, but a rising share (evident 1979-1991) reflects career and procurement self-perpetuation once technical parity was largely achieved. Accessibility collapse is moderate (0.4) — states retain some room to adopt alternative postures (minimum deterrence, no-first-use) even while operating inside the reading's logic; resistance (0.58) is substantial, from arms-control movements, non-nuclear coalitions, and periodic domestic backlash against modernization budgets.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear-weapons-states seat, the arrangement is rational coordination solving a genuine mutual-preemption problem — the doctrine apparatus is the coordination mechanism itself. From the payer seats (taxpayers, non-nuclear states, proxy-conflict populations), the same structure computes as extraction: resources and risk flow upward and outward while the payers have no seat in doctrine formation. This divergence is exactly what the tangled_rope classification is built to hold — both the coordination function and the asymmetric extraction are real and coexist through the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-weapons states and the industrial/academic beneficiaries sit near the beneficiary end of directionality: they set the doctrine, capture the procurement and career rents, and retain arbitrage-grade exit (they can shift posture, negotiate, or escalate at will). Non-nuclear states and proxy-conflict populations sit near the target end: constrained or trapped exit, no control over the possibility space's boundaries, and they absorb risk generated by a competition dynamic they do not run. Domestic taxpayers are powerless and trapped — they fund the equilibrium without a plausible exit from the fiscal transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding total war among competing nuclear powers) is genuinely contested as live vs. dead: multipolar proliferation and new delivery technologies keep parts of it live, while the bulk of restraint plausibly now rests on normative taboo and structural space-contraction (the sibling readings) rather than on the continuously re-theorized doctrine apparatus this reading privileges. Classifying this reading as tangled_rope rather than mountain or pure rope prevents two mislabelings: treating the doctrine-and-procurement apparatus as pure natural necessity (which would hide the real extraction from taxpayers and proxy populations), and treating it as pure extraction (which would erase the genuine, non-trivial coordination function of making mutual vulnerability legible and credible).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_underdetermination,
    'Is the deterrence-equilibrium reading, the nuclear-taboo reading, or the space-contraction reading the structurally correct account of why total war has not recurred among nuclear powers since 1945 — or do all three operate simultaneously at different strengths?',
    'Comparative historical analysis of near-miss crises (Cuban Missile Crisis, 1983 Able Archer, India-Pakistan crises) for evidence of which mechanism was doing the restraining work at the moment of maximum strain — capability-cost calculation, normative horror, or literal unthinkability.',
    'If the taboo or space-contraction mechanisms are doing most of the restraining work, the continuous doctrine-and-procurement apparatus this reading justifies is substantially extractive theater riding on restraint achieved through other means; if the deterrence-equilibrium mechanism is load-bearing, the apparatus is closer to genuine, continuously necessary coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_underdetermination, conceptual, 'Which kernel reading actually does the restraining work is empirically and conceptually undetermined.').

omega_variable(
    doctrine_necessity_vs_self_perpetuation,
    'Does continued escalation-ladder theorization and counterforce doctrine development still serve a genuine crisis-stability function, or has it become primarily a self-perpetuating professional and industrial artifact?',
    'Track whether doctrine outputs (targeting revisions, escalation models) demonstrably alter crisis behavior in documented incidents, versus whether they primarily justify budget cycles and academic output independent of operational uptake.',
    'If doctrine output is decoupled from actual crisis behavior, the theater_ratio is understated and the constraint drifts further toward pure extraction (snare-adjacent) for the strategic-studies establishment; if tightly coupled, the tangled_rope coordination function is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_necessity_vs_self_perpetuation, empirical, 'Whether ongoing doctrine elaboration is functionally necessary or institutionally self-perpetuating.').

omega_variable(
    proxy_conflict_displacement_attribution,
    'Is the displacement of violence into proxy and sub-threshold conflict a genuine causal consequence of the deterrence equilibrium at the top of the ladder, or would comparable proxy conflict occur regardless of the great-power nuclear posture?',
    'Comparative analysis of proxy-conflict intensity across periods of tighter versus looser great-power nuclear competition, controlling for regional and decolonization dynamics independent of the nuclear dyad.',
    'If displacement is a genuine causal artifact of the equilibrium, the victim classification of proxy-conflict populations is well-grounded and extraction is substantially understated by the metric; if proxy conflict is largely independent, the extraction attributed to this reading should be revised downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proxy_conflict_displacement_attribution, empirical, 'Whether proxy-conflict costs are properly attributable to this reading''s mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1962, 0.3).
narrative_ontology:measurement(tota_tr_t1979, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1979, 0.38).
narrative_ontology:measurement(tota_tr_t1991, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1991, 0.5).
narrative_ontology:measurement(tota_tr_t2008, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2008, 0.44).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1962, 0.55).
narrative_ontology:measurement(tota_be_t1979, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1979, 0.6).
narrative_ontology:measurement(tota_be_t1991, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1991, 0.45).
narrative_ontology:measurement(tota_be_t2008, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2008, 0.5).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2025, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1945, 0.3).
narrative_ontology:measurement(tota_su_t1962, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1962, 0.62).
narrative_ontology:measurement(tota_su_t1979, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1979, 0.58).
narrative_ontology:measurement(tota_su_t1991, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1991, 0.4).
narrative_ontology:measurement(tota_su_t2008, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2008, 0.45).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__deterrence_equilibrium_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the total_war_possibility_space kernel. deterrence_equilibrium_reading (this file) locates restraint in continuous cost-benefit calculation and predicts ongoing doctrine/procurement investment as functionally necessary. space_contraction_reading locates restraint in the removal of total war from thinkable strategic space, predicting doctrine atrophy rather than elaboration. nuclear_taboo_reading locates restraint in constructed normative prohibition independent of material capability, predicting that even capability parity changes would not restore total war to live consideration. The three share the same historical explanandum (no great-power total war since 1945) but diverge on mechanism, epsilon, and beneficiary structure — they are linked here rather than merged, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
