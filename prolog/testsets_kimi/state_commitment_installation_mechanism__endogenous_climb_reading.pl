% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__endogenous_climb_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: state_commitment_installation_mechanism__endogenous_climb_reading
 *   human_readable: Endogenous Climb Installation of State Commitments
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint instantiates the endogenous_climb_reading of the
 *   state_commitment_installation_mechanism kernel. It models the
 *   sociological claim that new state commitments and cultural norms gain
 *   legitimacy not through top-down mandate but through a climb from
 *   institutional fringes toward the apex, driven by demonstrated superiority
 *   and grassroots advocacy. The kernel is contested: the
 *   exogenous_imposition_reading holds that legitimacy is installed by
 *   authority holding a transformation mandate, while the
 *   hybrid_cascade_reading synthesizes both directions. This reading treats
 *   fringe actors as structural beneficiaries, apex incumbents as victims of
 *   displaced authority, and the climb itself as requiring active enforcement
 *   by advocacy coalitions.
 *
 * KEY AGENTS:
 *   - fringe_innovators: Primary beneficiaries (moderate/regional/mobile) â gain legitimacy as their commitments climb
 *   - grassroots_advocates: Agenda setters (organized/national/mobile) â provide the active enforcement of the climb
 *   - apex_incumbents: Primary payers (institutional/national/constrained) â bear extraction through displaced authority and legitimacy
 *   - early_adopter_intermediaries: Secondary beneficiaries (moderate/regional/mobile) â broker between fringe and apex
 *   - historical_sociologist: Analytical observer (analytical/global/analytical) â compares readings across the kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, 0.62).
domain_priors:suppression_score(state_commitment_installation_mechanism__endogenous_climb_reading, 0.48).
domain_priors:theater_ratio(state_commitment_installation_mechanism__endogenous_climb_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__endogenous_climb_reading, "Endogenous Climb Installation of State Commitments").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__endogenous_climb_reading, '909c52bc-7d8f-493b-a820-1f480bc93d33').
narrative_ontology:cs_kernel_codification('909c52bc-7d8f-493b-a820-1f480bc93d33', distributed).
narrative_ontology:cs_authority_grounding('909c52bc-7d8f-493b-a820-1f480bc93d33', expertise).
narrative_ontology:cs_interpretation_layer_present('909c52bc-7d8f-493b-a820-1f480bc93d33').
narrative_ontology:cs_reading_relation('909c52bc-7d8f-493b-a820-1f480bc93d33', state_commitment_installation_mechanism__exogenous_imposition_reading, coexists_with).
narrative_ontology:cs_reading_relation('909c52bc-7d8f-493b-a820-1f480bc93d33', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('909c52bc-7d8f-493b-a820-1f480bc93d33', foundational, legitimacy_from_demonstrated_superiority).
narrative_ontology:cs_axiom_status(legitimacy_from_demonstrated_superiority, holdable).
narrative_ontology:cs_axiom_grounding('909c52bc-7d8f-493b-a820-1f480bc93d33', legitimacy_from_demonstrated_superiority, empirically_contingent).
narrative_ontology:cs_axiom('909c52bc-7d8f-493b-a820-1f480bc93d33', foundational, bottom_up_installation_priority).
narrative_ontology:cs_axiom_status(bottom_up_installation_priority, holdable).
narrative_ontology:cs_axiom_grounding('909c52bc-7d8f-493b-a820-1f480bc93d33', bottom_up_installation_priority, empirically_contingent).
narrative_ontology:cs_reference_frame('909c52bc-7d8f-493b-a820-1f480bc93d33', fringe_demonstration_reference).
narrative_ontology:cs_drift_state('909c52bc-7d8f-493b-a820-1f480bc93d33', post_state_centered_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('909c52bc-7d8f-493b-a820-1f480bc93d33', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_innovators).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, grassroots_advocates).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopter_intermediaries).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, apex_incumbents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop novel practices, norms, or institutional forms at the margins of established authority. Their commitments gain legitimacy and diffuse upward as they demonstrate superior efficacy compared to apex arrangements, transforming local innovations into candidates for universal adoption.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_innovators, beneficiary,
    moderate, biographical, mobile, regional).

% Actively organize coalitions, produce demonstrations, and narrate the superiority of fringe commitments to intermediate and apex audiences. Their sustained advocacy is the enforcement machinery that pushes the commitment up institutional hierarchies against apex resistance.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, grassroots_advocates, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, grassroots_advocates, beneficiary).

% Occupy the center of established institutional authority and bear the cost of displaced legitimacy as fringe innovations climb. They actively resist the erosion of their commitments but are gradually overridden by the accumulating evidence and coalition pressure from below.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, apex_incumbents, payer,
    institutional, generational, constrained, national).

% Bridge fringe and apex by validating and adopting innovations before full institutional acceptance. They gain status, brokerage positions, and influence as translators between marginal experiments and central authority.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopter_intermediaries, beneficiary,
    moderate, biographical, mobile, regional).

% Documents and theorizes the climb mechanism across historical cases of state formation and institutional change. Compares endogenous patterns against exogenous and hybrid alternatives, providing the analytical seat that observes the structural asymmetry between fringe beneficiaries and apex victims.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, historical_sociologist, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_innovators).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how complex societies generate legitimate new commitments when no central authority exists to mandate them, or when top-down imposition is costly, contested, or infeasible. Coordinates the transfer of legitimacy from margins to center through iterated demonstration and advocacy.
% TRANSFER_FUNCTION: Moves legitimacy, authority, and institutional resources from established apex actors to fringe innovators and their coalitions. Transfers credibility upward from local demonstrations to universal norms, and transfers the costs of displacement onto incumbents who lose standing as their commitments are superseded.
% ABSENT_VOICES: Apex incumbents whose resistance is structurally discounted as mere conservatism; exogenous imposition theorists who argue that most legitimate commitments are actually installed top-down and that the fringe-climb narrative is a retrospective myth; and populations excluded from both fringe and apex who never gain voice in either mechanism.
% DISAPPEARANCE_RATIONALE: If legitimacy could only be installed through top-down imposition, the entire landscape of gradual institutional change would reorganize around revolution, mandate, or stagnation. The historical sociology of state formation, social movements, and cultural diffusion would require different theoretical foundations, and the beneficiaries of bottom-up innovation would lose their primary pathway to authority.
% FOUNDING_PROBLEM: How do new norms, practices, and state commitments become legitimate in complex societies when centralized imposition is unavailable, incomplete, or actively resisted by the existing institutional structure?
% FOUNDING_PROBLEM_CORROBORATION: Comparative historical sociologists outside the benefiting fringe document endogenous climb patterns across state formation cases; institutional historians note repeated episodes where top-down imposition failed and marginal innovations eventually captured the center. State-centered theorists contest the reading, arguing that the founding problem is better solved by exogenous imposition and that apparent climbs are epiphenomenal.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderately high because the mechanism systematically transfers legitimacy and authority from apex to fringe; it is not pure extraction because the coordination function (solving commitment installation without central imposition) is genuine. Suppression (0.48) reflects the active work of excluding alternative installation mechanisms and overcoming apex resistance, not merely passive diffusion. Theater ratio (0.28) is moderate: grassroots advocacy is largely functional but carries increasing performative load as the coalition scales. Resistance (0.72) is high because apex incumbents structurally resist displacement. Accessibility collapse (0.58) is moderate: once the endogenous climb model dominates a field, alternative installation narratives become harder to articulate.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (grassroots advocates) experiences the constraint as genuine coordinationâthey are solving the hard problem of generating legitimacy without a mandate. The payer seat (apex incumbents) experiences the same structure as extractive displacement. The beneficiary seat (fringe innovators) sees a meritocratic pathway. The engine computes this divergence from the structural asymmetry in exit options and power; the authored claim (tangled_rope) does not adjudicate between these seats but names the hybrid structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Fringe innovators and grassroots advocates sit near the beneficiary end: they collect legitimacy, authority, and institutional standing from the constraint's operation. Their mobile exit options reflect that they can abandon failed innovations or move to new margins. Apex incumbents sit near the full-target end: they pay through displaced authority and constrained exit, locked into institutional positions that are devalued as the climb proceeds. Early adopters sit between, gaining brokerage benefits. The engine will derive high d for institutional payers with constrained exit and low d for mobile fringe beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare preserves the genuine coordination function: societies do face the problem of how to legitimate new commitments, and endogenous climb is one solution. A snare reading would require that the coordination story be pure cover, which is not descriptively accurateâdemonstrated superiority is a real (if contested) filter. A rope reading would ignore the asymmetric extraction that falls on apex incumbents and the active enforcement required to overcome their resistance. Tangled_rope captures both the coordination and the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_empirical_priority,
    'Does the endogenous climb reading describe the dominant historical pathway for commitment installation, or is it one path among several equally common mechanisms?',
    'Large-N comparative historical analysis counting installation episodes by mechanism type, controlling for state capacity and communications technology.',
    'If exogenous imposition is historically dominant, this reading is a false generalization and should compute as higher extraction (snare-like) because it vindicates fringe actors at the expense of descriptive accuracy. If endogenous climb is dominant, the coordination function is empirically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_empirical_priority, empirical, 'Whether this kernel reading or its siblings better describes historical state formation').

omega_variable(
    demonstrated_superiority_vs_retrospective_construction,
    'Is the demonstrated superiority that drives the climb identifiable in real time, or is it retrospectively constructed by winners after the climb succeeds?',
    'Process-tracing of historical cases with contemporary documentary evidence from before the outcome was certain; comparison of contemporaneous assessments versus post-hoc narratives.',
    'If superiority is largely retrospective, the constraint''s coordination function is weaker than claimed and the mechanism operates more like a snareâextracting authority from apex incumbents based on narrated rather than real superiority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrated_superiority_vs_retrospective_construction, empirical, 'Whether fringe superiority is real-time selected or Whig-historian constructed').

omega_variable(
    apex_resistance_mechanism,
    'Is apex resistance to the climb a genuine structural defense of functional commitments, or a performative maintenance of authority by incumbents who know their position is vulnerable?',
    'Comparative analysis of resistance outcomes: does resistance correlate with objective performance gaps between fringe and apex commitments, or with incumbent tenure and institutional entrenchment regardless of performance?',
    'If resistance is primarily performative, the theater_ratio should be higher and the mechanism''s extractiveness is amplified by theatrical maintenance of obsolete authority. If resistance is functional, the coordination-extraction balance shifts toward genuine problem-solving.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(apex_resistance_mechanism, conceptual, 'Whether apex resistance is structural or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t8, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(stat_tr_t16, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(stat_tr_t24, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(stat_tr_t32, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 40, 0.32).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(stat_be_t8, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(stat_be_t16, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(stat_be_t24, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(stat_be_t32, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(stat_su_t8, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(stat_su_t16, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(stat_su_t24, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(stat_su_t32, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 32, 0.52).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__endogenous_climb_reading, 0.08).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, exogenous_imposition_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the state_commitment_installation_mechanism kernel, which decomposes into three structurally distinct claims about how new commitments gain legitimacy: endogenous climb (fringe-up), exogenous imposition (apex-down), and hybrid cascade (both). Each reading has a different Îµ, beneficiary structure, and empirical profile. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
