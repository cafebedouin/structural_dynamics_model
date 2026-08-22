% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__competence_reading, []).

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
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint models preparedness as live exercised knowledge — the
 *   claim that operational capacity is maintained across generations through
 *   routines that genuinely test decision-making under uncertainty. Drills,
 *   exercises, mentorship, and after-action reviews constitute a coordination
 *   mechanism that transmits tacit knowledge and validates adaptive capacity.
 *   The constraint is claimed as a rope: it solves a real coordination
 *   problem (maintaining crisis competence across personnel turnover) with
 *   minimal extraction, and participants are net beneficiaries. The
 *   competence_reading asserts that the D5 break (where exercises become
 *   detached from reality) is avoided or contained through design.
 *
 * KEY AGENTS:
 *   - operational_personnel: Primary beneficiaries (moderate power, constrained exit) — gain competence and survival probability from exercised routines
 *   - affected_populations: Ultimate beneficiaries (powerless, trapped) — protected by maintained operational capacity
 *   - institutional_memory_holders: Dual role agenda_setter/beneficiary (organized, biographical) — design and transmit exercises, gain professional legitimacy
 *   - training_infrastructure: Organizational enablers (institutional, generational) — provide the material and structural substrate for exercises
 *   - external_auditors: Observers (analytical, analytical) — validate exercise realism and knowledge transfer efficacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.12).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "disaster_preparedness/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, '54fac293-7129-446e-b227-42c796dfe21d').
narrative_ontology:cs_kernel_codification('54fac293-7129-446e-b227-42c796dfe21d', distributed).
narrative_ontology:cs_authority_grounding('54fac293-7129-446e-b227-42c796dfe21d', practice).
narrative_ontology:cs_interpretation_layer_present('54fac293-7129-446e-b227-42c796dfe21d').
narrative_ontology:cs_reading_relation('54fac293-7129-446e-b227-42c796dfe21d', preparedness_commitment__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('54fac293-7129-446e-b227-42c796dfe21d', preparedness_commitment__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('54fac293-7129-446e-b227-42c796dfe21d', foundational, exercised_competence_is_necessary_and_sufficient_for_preparedness).
narrative_ontology:cs_axiom_status(exercised_competence_is_necessary_and_sufficient_for_preparedness, holdable).
narrative_ontology:cs_axiom_grounding('54fac293-7129-446e-b227-42c796dfe21d', exercised_competence_is_necessary_and_sufficient_for_preparedness, empirically_contingent).
narrative_ontology:cs_axiom('54fac293-7129-446e-b227-42c796dfe21d', secondary, memorial_performance_without_competence_is_harmful_deception).
narrative_ontology:cs_axiom_status(memorial_performance_without_competence_is_harmful_deception, holdable).
narrative_ontology:cs_axiom_grounding('54fac293-7129-446e-b227-42c796dfe21d', memorial_performance_without_competence_is_harmful_deception, deontological).
narrative_ontology:cs_reference_frame('54fac293-7129-446e-b227-42c796dfe21d', live_practice_transmission).
narrative_ontology:cs_drift_state('54fac293-7129-446e-b227-42c796dfe21d', contemporary_high_tempo_operations, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('54fac293-7129-446e-b227-42c796dfe21d', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, operational_personnel).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, affected_populations).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, institutional_memory_holders).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, competence_based_authority_legitimacy).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, generational_transmission_through_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Frontline responders, operators, and decision-makers who participate in exercises, drills, and mentorship. They gain crisis competence, decision-making practice, and survival probability. Exit is constrained by professional commitment and specialized training investment; leaving means abandoning a career identity built around the practice.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, operational_personnel, beneficiary,
    moderate, biographical, constrained, national).

% Communities and individuals protected by the maintained operational capacity. They have no direct role in the exercises, no voice in their design, and cannot exit the risk environment. They are pure beneficiaries of the constraint's coordination function.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, affected_populations, beneficiary,
    powerless, biographical, trapped, regional).

% Senior practitioners, trainers, and doctrine writers who design exercises, curate after-action reviews, and mentor juniors. They set the agenda for what scenarios are exercised and how competence is assessed. They benefit professionally — their authority and identity are constituted through the practice. Exit is mobile: they can move to advisory roles or other institutions.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, institutional_memory_holders, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, institutional_memory_holders, beneficiary).

% The organizational units (training commands, simulation centers, exercise design cells) that provide the material and structural substrate for exercises. They allocate resources, maintain simulation fidelity, and determine exercise tempo. They have arbitrage-grade exit: the institution can reallocate funding to other priorities if the exercises are deemed low-value.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, training_infrastructure, agenda_setter,
    institutional, generational, arbitrage, national).

% After-action review boards, congressional oversight, academic researchers, and international peers who evaluate exercise realism and knowledge transfer efficacy. They neither collect nor pay; they validate whether the competence claim holds.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, external_auditors, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains operational crisis competence across generational turnover through live exercises that test real decision-making under uncertainty, mentorship that transmits tacit knowledge, and after-action reviews that validate and update doctrine.
% TRANSFER_FUNCTION: Moves time, attention, and institutional resources from training infrastructure and operational personnel into exercised competence that protects affected populations. No monetary extraction; the transfer is effort-for-capability.
% ABSENT_VOICES: Affected populations are structurally excluded from exercise design and doctrine formation — they would object to exercises that prioritize institutional convenience over their protection, but they have no seat at the table. Future generations (unborn) are also absent; they inherit the competence level the current generation transmits.
% DISAPPEARANCE_RATIONALE: If live exercised knowledge routines vanished overnight, crisis competence would degrade rapidly with each generational turnover. Tacit knowledge would not be transmitted, decision-making under uncertainty would revert to untrained improvisation, and affected populations would lose the protective infrastructure. The world would rearrange toward higher casualty rates and slower adaptation.
% FOUNDING_PROBLEM: Early disaster response organizations found that paper plans and classroom training failed catastrophically in real events — personnel froze, communications collapsed, and tacit knowledge died with retiring veterans. The founding problem was: how to maintain crisis competence across generations when real crises are rare but catastrophic.
% FOUNDING_PROBLEM_CORROBORATION: After-action reviews from major disasters (e.g., Hurricane Katrina, Fukushima, COVID-19) consistently identify exercised competence gaps as failure drivers. Independent commissions (9/11 Commission, Kemeny Commission, various national audit bodies) attest the problem persists. The competence_reading's beneficiaries (operational personnel, institutional_memory_holders) confirm the problem is live; no major institutional voice claims it is solved.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__competence_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__competence_reading_tests).
:- end_tests(preparedness_commitment__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.18) because the arrangement's primary function is coordination — maintaining crisis competence across generations — and the costs (time in exercises, training infrastructure) are broadly shared and reciprocated by survival benefits. Suppression is low (0.12) because participation is largely voluntary within professional roles and alternatives (different training doctrines) are not actively suppressed. Theater ratio is modest (0.22) — some exercises drift toward performance, but the reading claims this is contained. Accessibility collapse is moderate (0.35) — alternative preparedness models exist but are less validated. Resistance is moderate (0.42) — organizations resist exercise tempo and resource demands, but not the fundamental premise. The constraint does not require active enforcement; it persists because it works.
 *
 * PERSPECTIVAL GAP:
 *   From the operational personnel seat, the constraint is experienced as genuine skill-building with reciprocal benefit — a rope. From the affected population seat, it is an invisible protective infrastructure they cannot exit — beneficiary with no voice. From the institutional_memory_holder seat, it is both agenda-setting (they design the exercises) and beneficiary (their professional identity is constituted through the practice). The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Operational personnel and affected populations are structural beneficiaries (d near 0.0) — the constraint subsidizes their survival and competence. Institutional memory holders sit near symmetric (d ≈ 0.5) — they invest design effort and gain professional authority. Training infrastructure bears maintenance costs but is institutionally funded. No agent is a net extractee; the arrangement is not extractive in its competence_reading instantiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — maintaining crisis competence across generational turnover — remains live. The constraint has not outlived its function. Mandatrophy is resolved in the negative: the arrangement continues to solve the problem it was built for. The hybrid_reading suggests some drift toward memorial performance, but this reading asserts the core coordination function remains dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does this constraint instantiate the competence_reading of the preparedness_commitment kernel, and how would the husk_reading and hybrid_reading change the structural classification?',
    'Comparative analysis across the three readings: each reading authors its own ε, beneficiaries/victims, and metrics; the engine computes per-reading classifications. Divergence between readings is the measurement the kernel structure exists to take.',
    'If competence_reading computes as rope (genuine coordination) while husk_reading computes as piton or snare, the kernel itself is not a single constraint — it is a family of structurally distinct constraints linked by network.affects_constraints. The omega documents this framing under-determination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment kernel framing: competence vs. husk vs. hybrid readings').

omega_variable(
    competence_measurement_validity,
    'Do the exercises and drills that constitute this constraint actually test real decision-making under uncertainty, or do they measure proxy performance that correlates poorly with crisis competence?',
    'After-action reviews from real events comparing units with high drill scores vs. actual crisis performance; longitudinal tracking of decision-quality metrics in exercises vs. operational outcomes.',
    'If drills are proxy theater, extractiveness and theater_ratio are understated — the constraint would reclassify toward piton or tangled_rope. If drills are valid, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_measurement_validity, empirical, 'Whether exercised knowledge drills measure real crisis competence or proxy performance').

omega_variable(
    generational_turnover_absorption,
    'Can the training and mentorship system genuinely absorb generational turnover without loss of tacit knowledge, or does each turnover cycle degrade the constraint''s coordination function?',
    'Cohort studies tracking knowledge retention across 2-3 generational transitions in high-hazard organizations; comparison of error rates and adaptation speed in units with different mentorship structures.',
    'If turnover causes systematic degradation, the constraint accumulates extraction over time (rising base_extractiveness) and the D5 break becomes probable — shifting classification toward piton or snare. Stable absorption supports the rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_turnover_absorption, empirical, 'Whether generational turnover is absorbed without degradation of operational capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(prep_tr_t5, preparedness_commitment__competence_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__competence_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(prep_tr_t15, preparedness_commitment__competence_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__competence_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(prep_tr_t25, preparedness_commitment__competence_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__competence_reading, theater_ratio, 30, 0.22).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(prep_be_t5, preparedness_commitment__competence_reading, base_extractiveness, 5, 0.16).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__competence_reading, base_extractiveness, 10, 0.17).
narrative_ontology:measurement(prep_be_t15, preparedness_commitment__competence_reading, base_extractiveness, 15, 0.17).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__competence_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(prep_be_t25, preparedness_commitment__competence_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__competence_reading, base_extractiveness, 30, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_commitment__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__competence_reading, 0.1).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the preparedness_commitment kernel. The competence_reading claims genuine coordination (rope); the husk_reading claims memorial performance with degraded function (likely piton or snare); the hybrid_reading claims a layered system with both coordination and performance elements (likely tangled_rope). They are linked via network.affects_constraints to enable contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
