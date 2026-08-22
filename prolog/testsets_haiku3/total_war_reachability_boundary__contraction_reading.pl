% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contraction_reading, []).

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
    narrative_ontology:measurement_basis/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Total War Reachability Contraction (Nuclear MAD Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   Under the contraction reading, nuclear weapons and mutual assured
 *   destruction (MAD) produced a permanent, irreversible contraction of the
 *   strategic space: total war — the attempt by one great power to eliminate
 *   another's capacity to resist and capture or dictate terms — is no longer
 *   reachable as a feasible strategy. Pre-nuclear great power conflict
 *   allowed (however destructively) one side to win militarily. Post-nuclear
 *   deterrence, given MAD dynamics, makes total war between major powers
 *   physically reachable but strategically equivalent to mutual annihilation
 *   — hence no longer a rational choice for either party. The constraint is
 *   claimed as a mountain: an emergent, natural feature of the current
 *   strategic environment, not a maintained human arrangement. No beneficiary
 *   structure exists because no party benefits from the constraint; both are
 *   harmed (unable to win), and the alternative (nuclear war) is worse. The
 *   victim set, if any, is universal (all humans face extinction risk if the
 *   constraint fails).
 *
 * KEY AGENTS:
 *   - major_nuclear_powers (powerless to escape the constraint, institutional, civilizational horizon, strategic_reachability_boundary is not a negotiable seat but a physical fact)
 *   - humanity_at_large (universal victim if the constraint fails, analytical seat)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.02).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.0).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Total War Reachability Contraction (Nuclear MAD Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, '851b769d-3e24-4ca5-b01b-f0dc511b74e6').
narrative_ontology:cs_kernel_codification('851b769d-3e24-4ca5-b01b-f0dc511b74e6', distributed).
narrative_ontology:cs_authority_grounding('851b769d-3e24-4ca5-b01b-f0dc511b74e6', expertise).
narrative_ontology:cs_interpretation_layer_present('851b769d-3e24-4ca5-b01b-f0dc511b74e6').
narrative_ontology:cs_reading_relation('851b769d-3e24-4ca5-b01b-f0dc511b74e6', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_reading_relation('851b769d-3e24-4ca5-b01b-f0dc511b74e6', total_war_reachability_boundary__contingent_reachability_reading, influences).
narrative_ontology:cs_axiom('851b769d-3e24-4ca5-b01b-f0dc511b74e6', foundational, mutual_assured_destruction_eliminates_winnable_strategies).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_eliminates_winnable_strategies, holdable).
narrative_ontology:cs_axiom_grounding('851b769d-3e24-4ca5-b01b-f0dc511b74e6', mutual_assured_destruction_eliminates_winnable_strategies, empirically_contingent).
narrative_ontology:cs_axiom('851b769d-3e24-4ca5-b01b-f0dc511b74e6', foundational, reachability_contraction_is_irreversible_given_current_arsenals).
narrative_ontology:cs_axiom_status(reachability_contraction_is_irreversible_given_current_arsenals, holdable).
narrative_ontology:cs_axiom_grounding('851b769d-3e24-4ca5-b01b-f0dc511b74e6', reachability_contraction_is_irreversible_given_current_arsenals, empirically_contingent).
narrative_ontology:cs_reference_frame('851b769d-3e24-4ca5-b01b-f0dc511b74e6', physics_of_mutual_annihilation).
narrative_ontology:cs_drift_state('851b769d-3e24-4ca5-b01b-f0dc511b74e6', contemporary_strategic_environment, gap(stable, severe, true)).
narrative_ontology:cs_created_at('851b769d-3e24-4ca5-b01b-f0dc511b74e6', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contraction_reading, mutually_assured_destruction_eliminates_winnable_total_war).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contraction_reading, strategic_reachability_contraction_via_weapons_technology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stakeholders authored EMPTY (Pattern-5: an explicit assertion that no
% entity's arrangements depend on this constraint — paired with the
% world_unchanged verdict below, enforced by the schema).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this reading frames total war reachability as a physical boundary, not a coordination solution.
% TRANSFER_FUNCTION: None — under this reading, the constraint is a natural law limiting what strategies remain feasible.
% ABSENT_VOICES: Strategic theorists holding the contingent_reachability or dropping readings remain in the conversation but are not presently organized as stakeholders (the constraint itself transcends institutional seats).
% DISAPPEARANCE_RATIONALE: The constraint is not a human arrangement that could disappear; it is asserted as a structural feature of the current strategic environment. If nuclear weapons disappeared, total war reachability would expand — but the constraint's statement is that given current weapons, total war is no longer strategically feasible (it reaches extinction). The constraint itself cannot vanish; only the material conditions that ground it can shift.
% FOUNDING_PROBLEM: Cold War strategy assumed great power total war remained a conceivable terminal option. Nuclear weapons and MAD theory contracted that strategic possibility — total war became physically reachable but strategically terminal (identical to non-existence for all parties).
% FOUNDING_PROBLEM_CORROBORATION: Strategic scholars across schools (Schelling, Brodie, Waltz, Jervis) converge that nuclear weapons altered the reachability of total war outcomes. The founding problem — 'can great powers still contemplate total war as a winning option?' — is unanimously answered no by mainstream deterrence theory and by the non-occurrence of US-USSR direct conflict despite decades of confrontation. Corroboration comes from outside any benefiting party (no party wins under this reading; no party has motive to fabricate the constraint). Military establishments and strategic analysts across rival powers acknowledge the binding force.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.02, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_reachability_boundary__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading treats total war reachability as a physical boundary, not a strategic choice or maintained coordination mechanism. Extractiveness is near zero (0.02) because the constraint is not extractive — it does not funnel resources to any beneficiary. Suppression is zero: there is no active suppression machinery; the constraint holds because of physics (mutual assured destruction), not enforcement. Theater ratio is zero: the constraint is not performative; its operation is deterministic. Accessibility collapse is very high (0.95): once the logic of MAD is understood, the alternatives to total war (all non-terminal strategies) become the only feasible set. Resistance is zero: no one is resisting a physical law. The measurements show extractiveness stable across the interval at or near 0.02 (minimal noise), reflecting the constraint's natural-law character — once MAD entered the strategic environment circa 1960, the reachability boundary stabilized. Theater and suppression remain at zero, consistent with mountain status. This reading does NOT author a beneficiary set because it asserts that no actor benefits from total war becoming unreachable — both major powers are harmed (unable to win), and smaller powers lose a 'great power will fight total war to protect us' guarantee.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the contraction reading, directionality is not applicable because no beneficiary or payer seats exist. The constraint is not an arrangement between actors; it is a feature of the physical environment all actors inhabit. The question 'who benefits?' receives no answer under this reading (which is exactly the diagnostic content: if one tried to name a beneficiary of 'total war is unreachable,' that claim would invoke a different reading, likely the false-summit variety). The absence of directionality is itself the reading's structural signature.
 *
 * MANDATROPHY ANALYSIS:
 *   The contraction reading avoids the false-summit problem (declaring beneficiaries on a mountain) by NOT authoring beneficiaries. The constraint is mountain-shaped: natural (MAD is a feature of physics + weapon technology), irreversible (given current arsenals), and not maintained by any human actor. The distinction from the dropping reading is critical: dropping holds that deterrence is a maintained rope (a coordination equilibrium that could break; proliferation or technological shift could restore reachability as a live option). Contraction holds that reachability has been structurally removed, not merely made costly. This mandatrophy difference is routed to the cs_structure layer: the contraction reading's reference frame is 'physics_of_mutual_annihilation' (a scientific reference, not a political or institutional one), while the dropping reading's reference frame would be 'deterrence_equilibrium' (a maintained institutional arrangement). The two readings forecast different drift profiles and different vulnerability to technological change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_piton_stability,
    'Is the total war reachability contraction a permanent structural feature (mountain), or is it a currently maintained but reversible institutional state (piton) that could degrade if enforcement (military spending, strategic doctrine, arms control) relaxes?',
    'Observe whether strategic reachability boundaries respond to changes in technology (space-based missile defense, hypersonic weapons, AI-enabled first-strike capability) or to changes in doctrine/political will (abandonment of MAD framework, adoption of counterforce targeting, nuclear war-fighting posture shifts). If reachability expands when technology shifts, the boundary is technology-contingent (supports contingent_reachability_reading). If reachability persists despite technology changes, the mountain reading holds.',
    'If mountain: total war reachability is irreversibly contracted; strategic deterrence is fundamentally secured by physics. If piton or contingent: the boundary could erode; deterrence is dependent on maintained military investment and political commitment. Strategic planning horizons change accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_piton_stability, empirical, 'Whether the reachability boundary is a natural law or a contingent institutional state.').

omega_variable(
    mountain_vs_rope_coordination,
    'Does the contraction reading conflate a natural physical boundary (no technical path to victory exists) with a maintained coordination equilibrium (all parties choose not to attempt total war even when paths might exist)?',
    'Analyze whether military-strategic planning operates under the assumption that total war is physically impossible or merely that it is mutually irrational given current arsenals. If planners treat reachability as contingent on continued deterrence posture (and would plan differently if deterrence failed), the constraint is more rope-like. If planners treat reachability as irreversibly closed (and would not plan total war even if deterrence doctrine were abandoned), the boundary is mountain-like.',
    'If coordination-dependent (rope): strategic policy depends on maintained deterrence agreements and doctrine; changes in nuclear strategy or proliferation alter reachability. If physics-dependent (mountain): reachability is independent of strategic choice; policy changes cannot restore total war as feasible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mountain_vs_rope_coordination, conceptual, 'Whether the boundary emerges from physics or from coordination equilibrium.').

omega_variable(
    beneficiary_invisibility_under_mountain_reading,
    'Does the contraction reading''s lack of declared beneficiaries represent a genuine mountain (no one benefits), or does it mask latent beneficiaries (major powers that benefit from the constraint by being unable to threaten each other with total annihilation)?',
    'Compare strategic security statements from major powers: do they frame the contraction of total war reachability as a loss (they can no longer threaten each other effectively) or as a gain (they are protected from existential threats)? If loss-framing dominates (dropping reading sentiment), beneficiaries are absent. If gain-framing dominates (protection from extinction), the reading is a false summit — beneficiaries exist but are not named.',
    'If false summit: the constraint is actually a snare or tangled rope with major powers as latent beneficiaries (protected from mutual annihilation). The extraction vector points toward smaller powers (who lose great-power-backed security guarantees) or toward states trying to build nuclear capacity (who find reachability contracted). If genuine mountain: no beneficiary structure exists; all parties are equally harmed by inability to win total war.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_invisibility_under_mountain_reading, conceptual, 'Whether the mountain reading conceals a beneficiary structure or genuinely lacks one.').

omega_variable(
    kernel_contest_sibling_foreclosure,
    'Do the three readings of the kernel (contraction, dropping, contingent_reachability) logically foreclose each other, or do they coexist as live positions that could all be adopted by different parties?',
    'Examine whether a state could simultaneously hold all three readings: believe (1) total war is currently unreachable [contraction], (2) deterrence equilibrium could fail [dropping], AND (3) the contraction is reversible via technology [contingent_reachability]. If yes, coexistence holds. If holding one reading logically prevents holding another, declare foreclosure.',
    'If coexistence: all three readings remain live strategic positions; different states and theorists can adopt different readings without contradiction. If foreclosure: one reading is the winning position; the others should be archived or reclassified. The engine computes foreclosure from cs_axiom_contradiction; this omega documents the empirical structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_sibling_foreclosure, conceptual, 'Whether the three sibling readings logically foreclose each other or coexist as live positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__contraction_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement_basis(tota_tr_t1945, observed).
narrative_ontology:measurement(tota_tr_t1960, total_war_reachability_boundary__contraction_reading, theater_ratio, 1960, 0.0).
narrative_ontology:measurement_basis(tota_tr_t1960, observed).
narrative_ontology:measurement(tota_tr_t1980, total_war_reachability_boundary__contraction_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement_basis(tota_tr_t1980, observed).
narrative_ontology:measurement(tota_tr_t2000, total_war_reachability_boundary__contraction_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement_basis(tota_tr_t2000, observed).
narrative_ontology:measurement(tota_tr_t2026, total_war_reachability_boundary__contraction_reading, theater_ratio, 2026, 0.0).
narrative_ontology:measurement_basis(tota_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1945, 0.0).
narrative_ontology:measurement_basis(tota_be_t1945, observed).
narrative_ontology:measurement(tota_be_t1960, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1960, 0.01).
narrative_ontology:measurement_basis(tota_be_t1960, projected).
narrative_ontology:measurement(tota_be_t1980, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1980, 0.02).
narrative_ontology:measurement_basis(tota_be_t1980, observed).
narrative_ontology:measurement(tota_be_t2000, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2000, 0.02).
narrative_ontology:measurement_basis(tota_be_t2000, observed).
narrative_ontology:measurement(tota_be_t2026, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2026, 0.02).
narrative_ontology:measurement_basis(tota_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_reachability_boundary__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% The contraction reading instantiates a kernel with three interpretations: contraction (this file) asserts total war reachability is a mountain (irreversibly contracted by MAD); dropping asserts it remains a rope (maintained deterrence equilibrium, reachable if deterrence fails); contingent_reachability asserts it is piton-like (temporarily contracted, reversible via technology). Each reading has a distinct ε, beneficiary structure, and strategic implications. All three readings share the same historical referent (nuclear weapons exist, deterrence doctrine operates, no US-USSR direct conflict occurred) and differ on what the referent means structurally. Network edges link the readings to enable cross-reading analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
