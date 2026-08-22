% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__real_catastrophe_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__real_catastrophe_only, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: competence_exercise_validity__real_catastrophe_only
 *   human_readable: Real Catastrophe as Sole Valid Competence Exercise
 *   domain: safety/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel:
 *   competence_exercise_validity. The reading asserts that only real
 *   catastrophe truly exercises and validates competence; simulation is an
 *   insufficient substitute that masks decay beneath compliance
 *   documentation. Under this reading, the institutional arrangement
 *   (extensive mandated simulation) extracts costs from operational personnel
 *   and safety-dependent populations by creating a false assurance of
 *   preparedness—the constraint persists because institutions benefit from
 *   the liability shield simulation provides, not because it demonstrably
 *   preserves competence. The constraint is claimed as tangled_rope
 *   (coordination function + asymmetric extraction + active enforcement)
 *   because it solves a real coordination problem (validating competence
 *   without waiting for disasters) while simultaneously extracting
 *   institutional authority and suppressing alternative validation
 *   frameworks. The sibling readings (simulation_as_proxy,
 *   continuous_refresh_hybrid) represent different institutional commitments
 *   to the same kernel: whether simulation counts as sufficient, necessary
 *   but insufficient, or only a proxy.
 *
 * KEY AGENTS:
 *   - operational_personnel: bears the cost of continuous simulation; exit is constrained (career change or jurisdiction exit)
 *   - safety_bureaucracy: sets and enforces the constraint; collects authority and risk-transfer
 *   - simulation_industry: benefits from sustained demand for training and technology
 *   - safety_dependent_populations: trapped beneficiary-payers (nominal safety benefit, real catastrophe cost when competence is unmeasured)
 *   - accident_investigators: analytical seat; hold evidence of competence gaps simulation masked
 *   - simulation_validation_researchers: excluded from validation authority despite evidence that simulation transfer is limited
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, 0.68).
domain_priors:suppression_score(competence_exercise_validity__real_catastrophe_only, 0.72).
domain_priors:theater_ratio(competence_exercise_validity__real_catastrophe_only, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__real_catastrophe_only, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__real_catastrophe_only, "Real Catastrophe as Sole Valid Competence Exercise").
narrative_ontology:topic_domain(competence_exercise_validity__real_catastrophe_only, "safety/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__real_catastrophe_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__real_catastrophe_only, '81fc605f-b04e-45e5-a196-7350601e5613').
narrative_ontology:cs_kernel_codification('81fc605f-b04e-45e5-a196-7350601e5613', distributed).
narrative_ontology:cs_authority_grounding('81fc605f-b04e-45e5-a196-7350601e5613', extraction).
narrative_ontology:cs_interpretation_layer_present('81fc605f-b04e-45e5-a196-7350601e5613').
narrative_ontology:cs_reading_relation('81fc605f-b04e-45e5-a196-7350601e5613', competence_exercise_validity__simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('81fc605f-b04e-45e5-a196-7350601e5613', competence_exercise_validity__continuous_refresh_hybrid, coexists_with).
narrative_ontology:cs_axiom('81fc605f-b04e-45e5-a196-7350601e5613', foundational, simulation_structural_insufficiency).
narrative_ontology:cs_axiom_status(simulation_structural_insufficiency, holdable).
narrative_ontology:cs_axiom_grounding('81fc605f-b04e-45e5-a196-7350601e5613', simulation_structural_insufficiency, empirically_contingent).
narrative_ontology:cs_axiom('81fc605f-b04e-45e5-a196-7350601e5613', foundational, catastrophe_as_true_validation).
narrative_ontology:cs_axiom_status(catastrophe_as_true_validation, holdable).
narrative_ontology:cs_axiom_grounding('81fc605f-b04e-45e5-a196-7350601e5613', catastrophe_as_true_validation, deontological).
narrative_ontology:cs_reference_frame('81fc605f-b04e-45e5-a196-7350601e5613', simulation_sufficiency_doctrine).
narrative_ontology:cs_drift_state('81fc605f-b04e-45e5-a196-7350601e5613', contemporary_post_accident_inquiry_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('81fc605f-b04e-45e5-a196-7350601e5613', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, safety_bureaucracy).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, simulation_industry).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, operational_personnel).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, safety_dependent_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, safety_dependent_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operators, engineers, and decision-makers in high-stakes domains (aviation, nuclear, emergency response, medical crisis management) who are required to participate in extensive simulation and drill programs. They bear the cost—time, cognitive load, opportunity cost, psychological wear from repeated false crises—without the certainty that this preparation will actually perform when a real event occurs. Their competence is only validated when catastrophe strikes; until then, simulation masquerades as proof but leaves decay undetected. Exit means changing careers or jurisdictions.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, operational_personnel, payer,
    moderate, biographical, constrained, national).

% Patients in hospitals, passengers on aircraft, residents near industrial facilities, populations in emergency zones—those whose safety depends on the competence of operational personnel. They pay when that competence was never actually validated (only simulated), and the real catastrophe occurs with personnel whose skills have decayed beneath simulation's masking performance. They also nominally benefit from the safety systems built, but benefit is contingent on actual competence, which remains unproven.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, safety_dependent_populations, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__real_catastrophe_only, safety_dependent_populations, beneficiary).

% Regulatory agencies, compliance bodies, and institutional safety officers who define, mandate, and enforce competence-validation frameworks. Under this reading (real-catastrophe-only), they have structural incentive to continue requiring extensive simulation because: (1) it generates compliance documentation that shifts liability away from the institution if failure occurs, (2) it defers the true competence test to the next catastrophe, (3) it keeps the validation machinery under institutional control rather than empirical falsification. They collect authority and risk-transfer from the arrangement.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, safety_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__real_catastrophe_only, safety_bureaucracy, beneficiary).

% Providers of simulation technology, training platforms, scenario development, and exercise infrastructure (companies, consultants, training centers, software vendors). They benefit directly from the constraint that simulation is required, continuous, and non-substitutable—the more the constraint persists, the more their services are purchased. The constraint's logic (simulation as competence exercise) justifies their revenue streams; they have structural incentive to suppress or muddy the empirical question of whether simulation actually preserves competence or merely performs it.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, simulation_industry, beneficiary,
    organized, biographical, mobile, global).

% Post-catastrophe investigation boards, regulatory inspectors, and independent safety researchers who examine failures. They hold the seat from which the true test emerges: did the real catastrophe reveal competence deficits that simulation had masked? They generate the evidence that could resolve whether simulation is sufficient, but they only have data AFTER the catastrophe has occurred—they cannot prevent the harm.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, accident_investigators, observer,
    institutional, biographical, analytical, national).

% Independent scientists and engineers who conduct empirical studies on simulation transfer (does training in simulators actually predict performance in real environments). Their findings often show modest transfer effects, decay under time pressure, and domain-specific gaps—evidence that would undermine the constraint's logic. They are structurally excluded from the validation decision-making because institutions relying on the simulation = competence equation have disincentive to elevate their findings to policy. Their voices enter only under external pressure (post-accident inquiry, litigation, regulatory reform).
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, simulation_validation_researchers, excluded,
    powerful, biographical, constrained, global).

% Alternative competence-validation approaches (continuous low-stakes testing, rapid-cycle refresher training, competence decay models, just-in-time learning systems, or hybrid frameworks that treat simulation as necessary but not sufficient). These frameworks would distribute the validation load differently, shift authority away from simulation gatekeepers, and potentially expose that many domains operate with unmeasured competence decay. They are excluded because the institutional machinery treats 'simulation is the validation' as settled, not negotiable.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, competing_validation_frameworks, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__real_catastrophe_only, safety_bureaucracy).
narrative_ontology:fixing_cost_class(competence_exercise_validity__real_catastrophe_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to coordinate a solution to the genuine problem: high-stakes domains have catastrophic failure modes and operational personnel cannot be trained by letting real disasters happen. Simulation creates a single, repeatable, controlled environment where competence can be exercised without lives at stake, ostensibly substituting for rare catastrophic events.
% TRANSFER_FUNCTION: Transfers the cost of competence validation (time, cognitive burden, psychological load, opportunity cost) from catastrophe victims (who would bear it if real events were the only test) to operational personnel and institutions (who bear the cost of simulation). Also transfers authority over competence definition from empirical evidence to institutional compliance frameworks. Transfers revenue from institutions to simulation vendors. Transfers liability away from institutions by documenting compliance.
% ABSENT_VOICES: Simulation transfer researchers, operators who suspect their skills have decayed beneath the surface, safety-dependent populations who cannot testify until harm occurs, competing validation frameworks that would distribute competence testing differently. These voices would argue that simulation is a necessary but insufficient proxy, that competence decay persists unmeasured, and that real-world catastrophe remains the only actual validation—making the constraint a false security arrangement.
% DISAPPEARANCE_RATIONALE: If the constraint vanished (real-catastrophe-only ceased being imposed as doctrine), institutions would immediately face pressure to develop alternative competence validation methods. Simulation would likely continue but would no longer carry the false weight of 'proven competence.' Operational roles would reorganize around empirically validated training, and the safety bureaucracy would either adopt hybrid frameworks or face liability for knowingly using insufficient validation. The simulation industry would contract or shift business models. The true test (catastrophic events) would continue to reveal competence gaps, but they would no longer be defended as 'sufficient simulation was in place.'
% FOUNDING_PROBLEM: High-stakes operational domains (aviation, nuclear power, emergency medicine, military command) cannot allow operators to learn by catastrophic failure. Some competence validation mechanism must exist that does not require real-world disasters.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards (NTSB, aviation safety bodies, nuclear regulatory commissions) consistently confirm that catastrophic failures occur despite extensive simulation records—operators had certification, drill completion, and compliance documentation but made errors revealed only under real-world stress. Independent research on simulation transfer (cited in academic literature on human factors, training science) documents that simulation effectiveness degrades over time and does not reliably predict real-world performance under novel conditions. Safety-dependent populations and operational personnel corroborate that the constraint persists despite evidence that simulation alone has not prevented failures.
narrative_ontology:disappearance_verdict(competence_exercise_validity__real_catastrophe_only, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__real_catastrophe_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__real_catastrophe_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__real_catastrophe_only, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__real_catastrophe_only, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__real_catastrophe_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__real_catastrophe_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over time (0.48 → 0.68 across the interval) because as simulation programs mature and consolidate, the constraint's enforcement machinery grows—more mandatory drills, more documentation, more institutional gatekeeping, more revenue for simulation vendors. Theater rises sharply (0.42 → 0.58) because the constraint increasingly performs the role of competence validation without performing the actual validation: compliance documentation accumulates while competence is never empirically tested until catastrophe. Suppression is high and stable (0.60 → 0.72) because the constraint actively excludes alternative validation frameworks and suppresses evidence (simulation transfer research, accident findings) that would contradict the doctrine. Accessibility_collapse is moderate (0.64) because operational personnel and institutions are partially trapped by regulation and social expectation, but alternative frameworks remain conceptually available and are advocated by excluded researchers. Resistance is moderate (0.52) because operational personnel and safety-dependent populations push back against the false assurance, but institutions have structural incentive to maintain the constraint and the simulation industry mobilizes to defend it. The measurement series shows the constraint tightening and theatricalizing over its lifecycle: extraction and theater both rise, suppression stabilizes at high level, indicating a mature constraint sustained by inertia and institutional capture rather than by genuine coordination function.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (safety_bureaucracy) experiences this as justified coordination: 'We are solving the problem of validating competence without catastrophe by mandating rigorous simulation.' From their seat, the constraint is governance, necessary, and defensive (liability protection). The payer seats (operational_personnel, safety_dependent_populations) experience it as false assurance: 'We are performing competence validation without actually validating competence; we are bearing costs for a mechanism that leaves our actual readiness unmeasured.' From their seats, the constraint is extractive performance masquerading as coordination. The engine computes this divergence from the structural data: the beneficiaries (safety_bureaucracy, simulation_industry) have high-directness pathways to extraction (regulatory authority, revenue capture); the payers have constrained exit and unmeasured costs; the excluded voices (simulation_validation_researchers) hold evidence that would collapse the constraint's legitimacy. The computed types will diverge: beneficiary seats may compute as rope (genuine coordination from their vantage); payer seats compute toward snare (extraction defended by suppression of counter-evidence).
 *
 * DIRECTIONALITY LOGIC:
 *   Safety_bureaucracy: d ≈ 0.1–0.2 (beneficiary, high power, exercises institutional authority, arbitrage exit—they can shift the constraint or defend it as needed; the constraint transfers risk to them from catastrophe but they capture regulatory authority and institutional continuity). Simulation_industry: d ≈ 0.05–0.15 (pure beneficiary, organized, mobile exit—they can compete or exit the market; the constraint sustains their revenue). Operational_personnel: d ≈ 0.75–0.85 (target, moderate power but constrained exit due to professional licensing and career path dependence; bear the cost of continuous simulation without empirical guarantee it preserves competence). Safety_dependent_populations: d ≈ 0.80–0.90 (full target, powerless, trapped exit, bear the cost when the false assurance fails in real catastrophe). Accident_investigators: d ≈ 0.5 (symmetric observer, institutional power, analytical exit; they have neither collection incentive nor constraint-bearing burden, but their evidence could dissolve the constraint). Simulation_validation_researchers: d ≈ 0.55–0.65 (near-target excluded voice, powerful through expertise but constrained exit from the policy domain; they bear reputational cost of challenging institutional doctrine and see their evidence systematically excluded).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits mandatrophy: the founding problem (validating competence without catastrophe) remains live and real, but the institutional arrangement (mandate simulation as sole validation) has ossified into a mechanism that performs validation documentation rather than actual validation. The problem is NOT solved—competence decay remains unmeasured under simulation's masking performance—but the constraint persists because it transfers liability and authority to institutions. The classification as tangled_rope (not snare) captures the fact that genuine coordination function exists (the founding problem is real), but active enforcement (suppression of alternative frameworks, exclusion of validation researchers, compliance documentation theater) makes the extraction asymmetric and coercive. If the constraint were pure snare, it would be extracting pure rent with no coordination value; here it is coordination that has metastasized into extraction because the coordination problem is never actually solved—only deferred to the next catastrophe.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_transfer_empirical_gap,
    'What is the true transfer effectiveness of simulation training to real-world performance under catastrophic conditions, and does it degrade over time in unmeasured ways?',
    'Systematic empirical study of simulator performance vs. real-catastrophe performance in matched cohorts; longitudinal tracking of competence decay curves; post-accident analysis of operator skill gaps correlated with time since last simulation.',
    'High transfer with low decay would support the beneficiary reading (simulation is sufficient); low transfer with high decay would support the victim reading (simulation masks unmeasured decay) and trigger mandate revision toward hybrid or continuous frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_empirical_gap, empirical, 'Gap between measured simulator performance and actual competence under catastrophic stress').

omega_variable(
    institutional_incentive_capture,
    'To what extent does the institutional benefit of simulation (liability shield, compliance documentation, authority over validation) override the institutional commitment to actual competence preservation?',
    'Analysis of accident investigation findings (do post-catastrophe reports identify competence gaps despite prior simulation compliance?); comparison of institutional behavior when evidence of simulation insufficiency emerges vs. when it is suppressed; observation of whether institutions adopt alternative frameworks when litigation or regulation mandates review.',
    'If capture is substantial, the constraint is correctly classified as tangled_rope or snare (extraction defended by suppression); if institutions genuinely prioritize competence, the constraint is rope (coordination with manageable overhead).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_incentive_capture, empirical, 'Whether institutional incentives align with actual competence or with documentation theater').

omega_variable(
    kernel_reading_falsification,
    'Can the real_catastrophe_only reading and the simulation_as_proxy reading coexist in a single institutional framework, or does one logically foreclose the other?',
    'Formal analysis of the two readings'' core premises: if simulation counts as validation (proxy reading), can it also be insufficient and mask decay (catastrophe-only reading)? If both hold, under what conditions?',
    'If logically compatible, the readings coexist_with (both are live institutional options). If incompatible, one forecloses the other and the kernel has irreducible structural indeterminacy. The classification of reading_relations depends on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_falsification, conceptual, 'Whether the two readings can coherently occupy the same commitment system').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.60–0.72) primarily structural (regulatory barriers, institutional gatekeeping, exclusion of validation researchers) or internalized (operators believe simulation is sufficient, institutions have internalized the doctrine as truth rather than strategic position)?',
    'Post-catastrophe analysis: if competence deficits are revealed despite comprehensive simulation records, do institutions immediately abandon simulation-centrism (structural suppression) or persist in defending it as valid (internalized belief)?',
    'If structural: suppression is enforced by external barriers and can be rapidly reversed by regulatory change. If internalized: suppression persists after barriers are removed and requires epistemic reorientation; the constraint''s effective suppression is higher than the scalar suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternative frameworks is imposed externally or believed internally').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__real_catastrophe_only, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__real_catastrophe_only, theater_ratio, 0, 0.42).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_validity__real_catastrophe_only, theater_ratio, 5, 0.46).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_validity__real_catastrophe_only, theater_ratio, 10, 0.5).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_validity__real_catastrophe_only, theater_ratio, 15, 0.54).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_validity__real_catastrophe_only, theater_ratio, 25, 0.57).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_validity__real_catastrophe_only, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(comp_be_t5, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(comp_be_t10, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(comp_be_t15, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(comp_be_t25, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(comp_be_t40, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(comp_su_t5, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(comp_su_t10, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(comp_su_t15, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(comp_su_t25, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(comp_su_t40, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__real_catastrophe_only, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__real_catastrophe_only, 0.18).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel competence_exercise_validity. All three stories share the founding problem (validating competence in high-stakes domains without letting disasters happen) but differ on what counts as validation. The real_catastrophe_only reading treats simulation as insufficient and extraction-laden; simulation_as_proxy treats simulation as sufficient exercise; continuous_refresh_hybrid treats it as necessary but insufficient. Constraint family decomposition per ε-invariance: each reading instantiates a different constraint with different ε (how much extraction the arrangement exhibits under each reading's assessment), different beneficiary/victim structures (who benefits from that reading's validation mechanism), and different structural relationships. All three are linked via network.affects_constraints so the contamination propagation engine can track how falsification of one reading affects the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_validity__real_catastrophe_only, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
