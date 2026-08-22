% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_fidelity_threshold, []).

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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
 *   human_readable: Simulation Fidelity Threshold for Competence Retention in High-Reliability Organizations
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel about whether
 *   simulation can substitute for real catastrophic experience in maintaining
 *   operator competence. The simulation_fidelity_threshold reading holds that
 *   sufficiency is neither categorically present (as the proxy-catastrophe
 *   reading claims) nor categorically absent (as the necessity reading
 *   claims), but is a technology-dependent threshold: below a certain
 *   fidelity, simulation trains only procedure; above it, simulation
 *   genuinely reproduces the stress/uncertainty structure of real catastrophe
 *   closely enough to transfer competence. This reading treats the
 *   threshold's existence and current technological reachability as an open
 *   engineering question, which is precisely what makes continued investment
 *   in simulation technology a coordination-rope rather than a settled
 *   solution or a false promise. The coordination function is real: pooling
 *   capital into shared simulation infrastructure lets an industry train for
 *   rare catastrophic scenarios without waiting for or manufacturing real
 *   ones.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.42).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.28).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "Simulation Fidelity Threshold for Competence Retention in High-Reliability Organizations").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "safety_engineering/organizational_learning").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, '4d943e7e-29f8-422e-9445-cf15718f6f3b').
narrative_ontology:cs_kernel_codification('4d943e7e-29f8-422e-9445-cf15718f6f3b', distributed).
narrative_ontology:cs_authority_grounding('4d943e7e-29f8-422e-9445-cf15718f6f3b', expertise).
narrative_ontology:cs_interpretation_layer_present('4d943e7e-29f8-422e-9445-cf15718f6f3b').
narrative_ontology:cs_reading_relation('4d943e7e-29f8-422e-9445-cf15718f6f3b', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d943e7e-29f8-422e-9445-cf15718f6f3b', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('4d943e7e-29f8-422e-9445-cf15718f6f3b', catastrophe_proxy_sufficiency__hybrid_degradation_reading, influences).
narrative_ontology:cs_axiom('4d943e7e-29f8-422e-9445-cf15718f6f3b', foundational, sufficiency_is_technology_indexed_not_categorical).
narrative_ontology:cs_axiom_status(sufficiency_is_technology_indexed_not_categorical, holdable).
narrative_ontology:cs_axiom_grounding('4d943e7e-29f8-422e-9445-cf15718f6f3b', sufficiency_is_technology_indexed_not_categorical, empirically_contingent).
narrative_ontology:cs_axiom('4d943e7e-29f8-422e-9445-cf15718f6f3b', secondary, fidelity_threshold_is_discrete_and_reachable).
narrative_ontology:cs_axiom_status(fidelity_threshold_is_discrete_and_reachable, holdable).
narrative_ontology:cs_axiom_grounding('4d943e7e-29f8-422e-9445-cf15718f6f3b', fidelity_threshold_is_discrete_and_reachable, empirically_contingent).
narrative_ontology:cs_reference_frame('4d943e7e-29f8-422e-9445-cf15718f6f3b', early_simulator_procedural_training_baseline).
narrative_ontology:cs_drift_state('4d943e7e-29f8-422e-9445-cf15718f6f3b', high_fidelity_digital_twin_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4d943e7e-29f8-422e-9445-cf15718f6f3b', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, training_program_administrators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, regulatory_certification_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, the_traveling_public).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, the_traveling_public).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, technology_dependent_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell high-fidelity simulators (full-motion flight decks, digital twin nuclear control rooms, VR mass-casualty triage suites) whose value proposition rests entirely on the claim that fidelity, once high enough, substitutes for real catastrophic stress. Revenue scales with organizations believing the threshold is reachable and worth continually re-investing to reach. They have no stake in the threshold being cheap or already met — the moving target sustains the market.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors, beneficiary,
    organized, generational, arbitrage, global).

% Design certification curricula and decide how much simulator time counts as competence-sufficient. They administer the threshold claim operationally, deciding what fidelity level to purchase and defend to oversight bodies. Their institutional legitimacy depends on being able to certify readiness without waiting for an actual catastrophe to test it.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, training_program_administrators, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, training_program_administrators, beneficiary).

% Pilots, control-room operators, and emergency responders whose actual competence under real catastrophic stress is what the threshold is a proxy for. They bear the consequence if the fidelity claim is wrong in either direction — over-certified operators who freeze under real stress the simulator never reproduced, or operators denied advancement because an arbitrarily set threshold judged them unready on a metric that doesn't track real performance. They cannot independently verify whether the simulator they trained on actually crossed the relevant threshold; they must take the administrators' word for it.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_operators, payer,
    moderate, biographical, constrained, national).

% Passengers, residents near nuclear or chemical facilities, and other downstream parties who benefit when the threshold claim is true (operators are genuinely prepared) and pay the cost when it is false (undetected competence gaps surface only during real catastrophe). They have no visibility into simulator fidelity specifications and no ability to demand higher standards directly.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, the_traveling_public, beneficiary,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, the_traveling_public, payer).

% Study near-miss data, post-incident reviews, and simulator validation studies to try to establish where the fidelity threshold actually sits for a given domain and technology generation. Their findings are the closest thing to an external check on vendor and administrator claims, but their access to proprietary simulator internals and incident data is often gatekept by the organizations they are evaluating.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, safety_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scarce, dangerous, and often irreversible catastrophic stress-response experience by allowing an organization to invest in simulation technology as a substitute for waiting for or inducing real catastrophes to train against. This solves a genuine problem: real catastrophes are too rare, too costly, and too dangerous to serve as a routine training mechanism.
% TRANSFER_FUNCTION: Moves capital from training budgets to simulation technology vendors, moves certification authority to administrators who set and interpret the threshold, and moves risk (of an uncalibrated threshold) onto frontline operators and the public who depend on their competence during an actual event.
% ABSENT_VOICES: Frontline operators rarely have a formal voice in setting what fidelity level counts as sufficient — that determination is made by administrators in consultation with vendors, not through operator testimony about what training scenarios actually felt inadequate under real pressure. Post-catastrophe survivors and families are also absent from threshold-setting despite being the ultimate evidence source for whether the threshold was met.
% DISAPPEARANCE_RATIONALE: If the simulation-sufficiency framework vanished, organizations would have no principled way to certify readiness short of live-catastrophe exposure or an accepted alternative (e.g., mandatory real-incident rotation, which most domains cannot ethically or economically provide). Training budgets, certification regimes, and the entire simulator manufacturing industry would need to reorganize around a different competence theory.
% FOUNDING_PROBLEM: High-reliability organizations (aviation, nuclear power, emergency medicine) needed a way to build and verify operator competence for catastrophic scenarios that are too rare, costly, or dangerous to practice on directly.
% FOUNDING_PROBLEM_CORROBORATION: Independent safety researchers and post-incident investigation boards (e.g., NTSB-style bodies, nuclear regulatory incident reviews) corroborate that the underlying problem — training for events that cannot be safely reproduced — remains genuinely unsolved and is not merely a pretext; these bodies are outside the vendor/administrator beneficiary set and their incident analyses are the primary evidence used to argue simulators are, or are not, crossing the needed fidelity threshold in specific domains.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).
:- end_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than low, because the fidelity threshold is not independently verifiable by the operators who depend on it — vendors and administrators jointly control both the technology roadmap and the criteria for declaring the threshold crossed, creating a soft principal-agent gap even though no party is a clean 'victim' in the tangled-rope sense. Suppression is low-moderate (0.28): operators are not coerced into accepting simulator adequacy claims through force, but their ability to independently benchmark fidelity against real catastrophic stress is structurally limited (they cannot ethically stage the comparison event). Theater ratio starts low and rises modestly (0.12 to 0.22) as certification programs increasingly point to hours-logged-in-simulator as a proxy metric for readiness, a mild Goodhart drift where the measurable substitutes for the target. Accessibility collapse is moderate (0.35): alternative competence-verification approaches (structured real-incident rotation, cross-organizational near-miss sharing) exist but are underused once simulation investment becomes the default path.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor and administrator seats, the constraint is straightforwardly rope: pooled technology investment solving a real, otherwise-unsolvable training problem. From the frontline-operator seat, the same structure can register as opaque — they are told the threshold is met but have no independent means to confirm this before the real event tests it. The engine should compute a divergence here without either seat being definitionally wrong; the divergence is the honest signature of an unverifiable-by-the-governed threshold.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation vendors sit closest to the beneficiary end: they profit from continued belief that the threshold is reachable-but-not-yet-reached, which sustains a permanent upgrade cycle. Training administrators are a secondary beneficiary/agenda-setter hybrid — their institutional legitimacy is served by having *a* defensible threshold claim, whatever its true fidelity. Frontline operators are the structural payers: they cannot verify the threshold independently and bear the consequences of miscalibration in either direction. The traveling/resident public is a downstream beneficiary-when-true, payer-when-false, with essentially no exit option (trapped exit_options) — they cannot select which airline's or plant's simulator training regime they are exposed to as a matter of daily life.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in both directions. Classifying it as pure extraction (snare) would miss that the coordination function — enabling training for catastrophic scenarios without recreating them — is genuine and technologically load-bearing; there is no serious alternative that doesn't involve either the same investment-and-threshold logic or the ethically unacceptable alternative of engineering real catastrophes. Classifying it as a pure mountain (given the language of a 'natural' fidelity threshold in physics/engineering) would miss that vendors and administrators actively shape where the threshold is declared crossed, and that the threshold's technology-dependence means it is a moving, negotiated target, not a fixed physical constant. Rope captures the moderate coordination-with-soft-asymmetry structure most accurately: the arrangement is a genuine coordination mechanism whose beneficiaries have a mild but real incentive to keep the threshold perpetually 'almost reached.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_reachability_by_domain,
    'For any given high-reliability domain (commercial aviation, nuclear operations, mass-casualty medicine), has current simulation technology actually crossed the fidelity threshold, or is the industry perpetually asymptotic to it?',
    'Longitudinal comparison of simulator-trained operator performance against operators with real-incident exposure, controlling for incident severity and simulator generation; requires access to proprietary incident and training data currently gatekept by vendors and administrators.',
    'If the threshold has been crossed in a given domain, continued heavy investment in ever-higher fidelity is itself a form of extraction dressed as necessary coordination. If it has not been crossed, the current certification regime may be systematically over-certifying operator readiness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_reachability_by_domain, empirical, 'Whether current simulation technology has actually reached the competence-sufficient fidelity threshold in specific domains.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the underlying phenomenon genuinely threshold-shaped (a discrete fidelity cutoff exists), or is this reading itself an artifact of wanting a tractable engineering target, when the true relationship between simulation fidelity and competence retention is continuous or reading-dependent as the sibling constraints (proxy-sufficient, necessity, hybrid-degradation) propose?',
    'Cross-domain meta-analysis testing whether competence-retention curves against fidelity investment show a genuine inflection (supporting threshold framing) or a smooth continuous relationship (undermining the binary-threshold premise distinctive to this reading).',
    'If no genuine inflection point exists, this reading''s core structural claim collapses toward the hybrid_degradation_reading or the proxy-sufficiency reading, materially changing which constraint best describes the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the threshold framing itself is a defensible structural claim or a convenient simplification of a continuous relationship.').

omega_variable(
    vendor_incentive_to_delay_threshold_declaration,
    'Do simulation technology vendors have a structural incentive to perpetually claim the threshold has ''almost'' but not quite been reached, in order to sustain upgrade-cycle revenue, independent of the actual state of the technology?',
    'Compare vendor public claims about fidelity milestones against independent researcher assessments over multiple technology generations; look for a pattern where the threshold is always declared one generation away.',
    'If confirmed, the beneficiary structure shifts from incidental (vendors benefit from a real coordination problem) toward mild manufactured-scarcity — moving this reading''s classification closer to a tangled_rope on continued investigation, even though this story author''s judgment places it at rope currently.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vendor_incentive_to_delay_threshold_declaration, empirical, 'Whether vendor claims about threshold proximity track real technological progress or a revenue-sustaining narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t8, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 8, 0.14).
narrative_ontology:measurement(cata_tr_t16, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 16, 0.17).
narrative_ontology:measurement(cata_tr_t24, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 24, 0.19).
narrative_ontology:measurement(cata_tr_t32, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 32, 0.21).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cata_be_t8, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(cata_be_t16, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(cata_be_t24, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(cata_be_t32, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 40, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resource_allocation).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.12).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, hybrid_degradation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the catastrophe_proxy_sufficiency kernel. simulation_as_proxy_catastrophe_reading claims categorical sufficiency (no threshold needed); catastrophe_necessity_reading claims categorical insufficiency (no threshold suffices); hybrid_degradation_reading claims a split between procedural and tacit competence with generational decay. This reading (simulation_fidelity_threshold) claims a technology-dependent binary threshold, positioning it structurally between the two categorical readings and orthogonal to the hybrid-degradation reading's temporal-decay framing. Each reading is authored as its own constraint with its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
