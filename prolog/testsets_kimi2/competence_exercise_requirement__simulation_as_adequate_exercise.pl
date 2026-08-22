% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__simulation_as_adequate_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__simulation_as_adequate_exercise, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: competence_exercise_requirement__simulation_as_adequate_exercise
 *   human_readable: Simulation-Only Competence Adequacy Claim
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested
 *   competence_exercise_requirement kernel: the claim that high-fidelity
 *   simulation with debriefing is structurally adequate to exercise the
 *   competence kernel in high-reliability organizations. The reading is
 *   validated institutionally by catastrophe-free decades and regulatory
 *   compliance metrics, but contested by readings that demand real-world
 *   catastrophe exposure or hybrid anchoring. The constraint coordinates
 *   scalable training and certification while potentially displacing risk
 *   onto frontline operators and the dependent public through inflated
 *   confidence in simulated readiness.
 *
 * KEY AGENTS:
 *   - regulatory_bodies: agenda_setter (institutional/constrained) â defines and enforces the simulation-adequacy standard
 *   - operator_organizations: primary beneficiary (institutional/constrained) â collects cost savings and scalable compliance
 *   - simulation_vendors: secondary beneficiary (powerful/mobile) â collects revenue from regulatory codification
 *   - frontline_operators: primary target (moderate/identity_locked) â bears the performance risk of transfer gaps
 *   - dependent_public: diffuse target (powerless/trapped) â bears uncompensated tail-risk of competence failure
 *   - safety_researchers: analytical observer (analytical/analytical) â documents transfer gaps but lacks institutional power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, 0.55).
domain_priors:suppression_score(competence_exercise_requirement__simulation_as_adequate_exercise, 0.48).
domain_priors:theater_ratio(competence_exercise_requirement__simulation_as_adequate_exercise, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, extractiveness, 0.55).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__simulation_as_adequate_exercise, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__simulation_as_adequate_exercise, "Simulation-Only Competence Adequacy Claim").
narrative_ontology:topic_domain(competence_exercise_requirement__simulation_as_adequate_exercise, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__simulation_as_adequate_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__simulation_as_adequate_exercise, 'afbde242-776d-4a74-be72-c89e4d342011').
narrative_ontology:cs_kernel_codification('afbde242-776d-4a74-be72-c89e4d342011', formalized).
narrative_ontology:cs_authority_grounding('afbde242-776d-4a74-be72-c89e4d342011', expertise).
narrative_ontology:cs_interpretation_layer_present('afbde242-776d-4a74-be72-c89e4d342011').
narrative_ontology:cs_reading_relation('afbde242-776d-4a74-be72-c89e4d342011', competence_exercise_requirement__catastrophe_as_necessary_anchor, coexists_with).
narrative_ontology:cs_reading_relation('afbde242-776d-4a74-be72-c89e4d342011', competence_exercise_requirement__hybrid_dependency, coexists_with).
narrative_ontology:cs_axiom('afbde242-776d-4a74-be72-c89e4d342011', foundational, simulation_constitutes_adequate_exercise).
narrative_ontology:cs_axiom_status(simulation_constitutes_adequate_exercise, holdable).
narrative_ontology:cs_axiom_grounding('afbde242-776d-4a74-be72-c89e4d342011', simulation_constitutes_adequate_exercise, empirically_contingent).
narrative_ontology:cs_axiom('afbde242-776d-4a74-be72-c89e4d342011', secondary, scheduled_debriefing_captures_operational_truth).
narrative_ontology:cs_axiom_status(scheduled_debriefing_captures_operational_truth, holdable).
narrative_ontology:cs_axiom_grounding('afbde242-776d-4a74-be72-c89e4d342011', scheduled_debriefing_captures_operational_truth, empirically_contingent).
narrative_ontology:cs_reference_frame('afbde242-776d-4a74-be72-c89e4d342011', scheduled_simulation_competence).
narrative_ontology:cs_drift_state('afbde242-776d-4a74-be72-c89e4d342011', contemporary_catastrophe_free_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('afbde242-776d-4a74-be72-c89e4d342011', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, operator_organizations).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_vendors).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, dependent_public).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_transfer_hypothesis).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, scheduled_competence_maintenance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define certification standards that count scheduled simulation and debriefing as adequate exercise of the competence kernel. They enforce compliance through licensing and audit regimes. Their legitimacy depends on preventing competence failures while keeping training economically feasible for operators.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Adopt simulation-centric training programs to satisfy regulatory requirements and maintain operator credentials. They benefit from predictable training costs, reduced operational downtime, and scalable compliance across large workforces. Their institutional survival depends on maintaining certification without requiring live system failures.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, operator_organizations, beneficiary,
    institutional, biographical, constrained, national).

% Develop and sell high-fidelity simulators and structured debriefing protocols. They benefit from regulatory codification that mandates recurring simulation hours. Their market expands as institutional acceptance of simulation sufficiency displaces alternative readiness models.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Complete mandatory simulation cycles to retain certification and employment. Their professional identity is fused to the credentials the system grants. They bear the existential and moral risk if simulated competence fails to transfer to rare catastrophic events for which they will be held accountable.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, frontline_operators, payer,
    moderate, biographical, identity_locked, national).

% Depend on operator competence during rare system failures in aviation, medicine, or industrial control. They have no practical exit from critical infrastructure and no voice in training-standard debates. They bear the uncompensated tail-risk of competence gaps that simulation failed to reproduce.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, dependent_public, payer,
    powerless, immediate, trapped, local).

% Study the transfer of training from simulated to operational environments, often documenting surprise breakdowns and competency gaps that scheduled drills did not prevent. Their findings are slower to integrate into standards than compliance checklists are to enforce.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, safety_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of competence maintenance during long catastrophe-free intervals by providing repeatable, scalable, standardized exercise of procedures and crew coordination without requiring live system failure or operational downtime.
% TRANSFER_FUNCTION: Moves the burden of competence validation from rare real-world operational exposure to scheduled, vendor-provided simulation cycles; moves institutional confidence from empirical catastrophe survival to documented training-hour compliance.
% ABSENT_VOICES: Frontline operators who experience surprise gaps between simulation and reality but whose identity is locked to the certification system; dependent public who suffer when competence fails but are structurally excluded from training debates; catastrophe-as-necessary-anchor advocates who argue no simulation reproduces the existential pressure of real jeopardy.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, certifying authorities would need alternative adequacy criteria; operator organizations would face vastly increased costs for live operational mentoring or catastrophe-exposure programs; simulation vendors would lose their regulatory market; training schedules would reorganize around empirical performance in live or high-jeopardy contexts rather than scheduled simulation.
% FOUNDING_PROBLEM: High-reliability organizations face decades between catastrophic events; without scheduled exercise, operator competence atrophies or never develops for rare failure modes; waiting for catastrophes to validate training is ethically and economically intolerable.
% FOUNDING_PROBLEM_CORROBORATION: Safety researchers and regulatory historians attest the founding problem from outside the benefiting parties; disaster investigation boards in aviation, nuclear, and medical domains repeatedly identify competence gaps in rare-event response as contributory factors, corroborating the need for structured exercise while remaining neutral on whether simulation alone is adequate.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__simulation_as_adequate_exercise, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__simulation_as_adequate_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__simulation_as_adequate_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is a tangled rope because it carries a genuine coordination functionâmaintaining procedural competence during long catastrophe-free intervalsâwhile also enabling asymmetric extraction. Organizations and vendors capture concentrated benefits (cost predictability, regulatory markets), whereas frontline operators and the public bear diffuse, deferred costs in the form of overcertified competence and undetected readiness gaps. Extractiveness (0.55) reflects the decoupling of simulation hours from empirical catastrophe performance; suppression (0.48) reflects the institutional marginalization of alternative readiness models; theater_ratio (0.28) captures the growing performative element of compliance-oriented drills. The temporal series show gradual extraction accumulation as catastrophe-free decades quiet dissent and harden the regulatory default.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory and operator seat, the constraint appears as a hard-won, evidence-based coordination mechanism that keeps industries safe without requiring disasters. From the frontline operator seat, it appears as an identity-locked certification treadmill that may overstate readiness. From the dependent-public seat, it is invisible until failure. The engine will compute divergent per-seat types because the structural asymmetry in exit options (institutional/constrained vs identity_locked/trapped) and directionalities (beneficiary vs payer) is stark.
 *
 * DIRECTIONALITY LOGIC:
 *   Operator organizations and simulation vendors are structural beneficiaries: they collect cost savings and revenue from the constraint's operation, situating them at the low-d end of the spectrum. Regulatory bodies sit near symmetricâthey enforce the constraint and bear reputational risk if it fails, but do not directly collect extraction. Frontline operators are targets: their professional identity is locked to the certification currency the constraint prints, giving them high effective extraction. The dependent public is the most diffuse target, with no exit and no voice, amplifying their effective extraction through scope effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâcompetence atrophy between rare catastrophesâremains live, which prevents a simple piton or snare classification. The constraint is not pure extraction because simulation genuinely coordinates some readiness maintenance. However, the 'adequacy' claim overreaches: it layers a rent-like risk displacement onto the coordination scaffold. Tangled rope captures this hybridity precisely, preventing misclassification as either benign rope (ignoring the risk displacement) or snare (ignoring the real coordination value of scheduled exercise).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the simulation-only reading of competence exercise foreclose the hybrid and catastrophe readings, or do all three coexist as structurally live positions?',
    'Comparative incident analysis: examine rare-event failures in organizations adhering to each reading to determine which structural arrangement produces superior operational outcomes.',
    'If the simulation reading is structurally inferior, its extraction profile rises because the risk displacement is demonstrably unwarranted; if equivalent, the contest is primarily preference-based.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Uncertainty about the relative structural validity of the three kernel readings').

omega_variable(
    simulation_transfer_validity,
    'Does high-fidelity simulation with debriefing actually transfer to competent performance in unscripted catastrophic events?',
    'Controlled empirical studies or natural experiments comparing crew performance in surprise scenarios between simulation-only and hybrid or catastrophe-exposed practitioners.',
    'If transfer is poor, the constraint''s base extractiveness is higher than authored because the confidence it sells is false; if transfer is strong, the extraction is largely coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_validity, empirical, 'Empirical uncertainty about skill transfer from simulation to real catastrophe').

omega_variable(
    false_confidence_mechanism,
    'Does the constraint generate performative confidence that suppresses alternative readiness investments independently of empirical transfer validity?',
    'Audit of training budgets and organizational attention: if catastrophe-free decades correlate with reduced investment in non-simulation readiness (mentorship, anomaly response, system redesign), the mechanism is performative confidence.',
    'If confirmed, theater_ratio underestimates the constraint''s performative load, and the effective suppression of alternatives is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_confidence_mechanism, empirical, 'Whether confidence in simulation suppresses complementary readiness investments').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__simulation_as_adequate_exercise, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sim_adeq_tr_t0, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sim_adeq_tr_t8, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 8, 0.12).
narrative_ontology:measurement(sim_adeq_tr_t16, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 16, 0.16).
narrative_ontology:measurement(sim_adeq_tr_t24, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 24, 0.2).
narrative_ontology:measurement(sim_adeq_tr_t32, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 32, 0.24).
narrative_ontology:measurement(sim_adeq_tr_t40, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(sim_adeq_be_t0, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(sim_adeq_be_t8, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(sim_adeq_be_t16, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(sim_adeq_be_t24, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(sim_adeq_be_t32, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 32, 0.52).
narrative_ontology:measurement(sim_adeq_be_t40, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 40, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(sim_adeq_su_t0, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(sim_adeq_su_t8, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 8, 0.25).
narrative_ontology:measurement(sim_adeq_su_t16, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 16, 0.32).
narrative_ontology:measurement(sim_adeq_su_t24, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(sim_adeq_su_t32, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 32, 0.44).
narrative_ontology:measurement(sim_adeq_su_t40, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__hybrid_dependency).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_exercise_requirement kernel, which decomposes into three structurally distinct claims about what constitutes adequate exercise of competence. This reading formalizes simulation sufficiency; sibling readings claim catastrophe necessity or hybrid dependency. The epsilon values differ because the referent changes: this story evaluates the simulation-only arrangement, not the same arrangement viewed differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
