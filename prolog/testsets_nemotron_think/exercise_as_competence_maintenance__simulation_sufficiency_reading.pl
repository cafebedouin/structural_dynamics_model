% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__simulation_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__simulation_sufficiency_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__simulation_sufficiency_reading
 *   human_readable: Simulation Sufficiency as Competence Maintenance
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   High-hazard industries (nuclear, chemical, aviation, offshore) are
 *   required by regulators to conduct regular catastrophe simulations as
 *   proof of organizational competence. The simulation_sufficiency_reading
 *   asserts that high-fidelity simulation genuinely exercises the same
 *   competence kernel that a real catastrophe would exercise — procedural
 *   fluency, decision-making under pressure, team coordination — and that
 *   retention effectiveness scales with simulation fidelity. Regulatory
 *   frameworks worldwide treat compliance with simulation mandates as
 *   sufficient evidence of preparedness. The victim set is narrowly defined:
 *   only those harmed when a specific simulation's fidelity was demonstrably
 *   inadequate. This reading underwrites a global compliance ecosystem of
 *   regulators, vendors, and auditors, while excluding voices that argue
 *   simulation cannot replicate the judgment-under-stakes component of
 *   competence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.58).
domain_priors:suppression_score(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.65).
domain_priors:theater_ratio(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__simulation_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__simulation_sufficiency_reading, "Simulation Sufficiency as Competence Maintenance").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__simulation_sufficiency_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__simulation_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'e2b16a91-a6e2-4841-a824-4db8c659549c').
narrative_ontology:cs_kernel_codification('e2b16a91-a6e2-4841-a824-4db8c659549c', formalized).
narrative_ontology:cs_authority_grounding('e2b16a91-a6e2-4841-a824-4db8c659549c', lineage).
narrative_ontology:cs_interpretation_layer_present('e2b16a91-a6e2-4841-a824-4db8c659549c').
narrative_ontology:cs_reading_relation('e2b16a91-a6e2-4841-a824-4db8c659549c', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('e2b16a91-a6e2-4841-a824-4db8c659549c', exercise_as_competence_maintenance__hybrid_decay_reading, coexists_with).
narrative_ontology:cs_axiom('e2b16a91-a6e2-4841-a824-4db8c659549c', foundational, simulation_fidelity_equals_competence_retention).
narrative_ontology:cs_axiom_status(simulation_fidelity_equals_competence_retention, holdable).
narrative_ontology:cs_axiom_grounding('e2b16a91-a6e2-4841-a824-4db8c659549c', simulation_fidelity_equals_competence_retention, empirically_contingent).
narrative_ontology:cs_axiom('e2b16a91-a6e2-4841-a824-4db8c659549c', foundational, competence_kernel_is_unitary_and_simulation_satisfiable).
narrative_ontology:cs_axiom_status(competence_kernel_is_unitary_and_simulation_satisfiable, holdable).
narrative_ontology:cs_axiom_grounding('e2b16a91-a6e2-4841-a824-4db8c659549c', competence_kernel_is_unitary_and_simulation_satisfiable, empirically_contingent).
narrative_ontology:cs_reference_frame('e2b16a91-a6e2-4841-a824-4db8c659549c', simulation_as_competence_exercise).
narrative_ontology:cs_drift_state('e2b16a91-a6e2-4841-a824-4db8c659549c', post_major_disaster_reveals_fidelity_gap, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e2b16a91-a6e2-4841-a824-4db8c659549c', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulated_organizations).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_vendors).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, compliance_auditors).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, public_at_risk).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_operators).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_compliance_adequacy).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__simulation_sufficiency_reading, measurable_readiness_substitutes_for_experience).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandate simulation-based drills as the primary legal proof of organizational preparedness. Write the standards that define 'sufficient fidelity' and accredit simulators. Their authority rests on the claim that regulated simulation exercises the same competence kernel as real events. They do not bear the consequences when simulation fidelity proves inadequate.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, safety_regulators, agenda_setter,
    institutional, generational, analytical, global).

% Satisfy regulatory mandates by running approved simulations rather than maintaining standing capability for rare catastrophes. Capture the cost difference between simulation budgets and the investment real readiness would require. Their exit from the simulation paradigm is constrained by liability regimes that treat compliance as a safe harbor.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulated_organizations, beneficiary,
    powerful, biographical, constrained, global).

% Sell scenario libraries, simulator time, and fidelity upgrades to regulated organizations. Their market exists because regulation defines competence in simulator-performance terms. They compete on fidelity metrics that regulators accept, not on demonstrated transfer to real-world judgment under stakes.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_vendors, beneficiary,
    organized, biographical, mobile, global).

% Certify that organizations have completed mandated simulation cycles at required fidelity levels. Their professional scope is defined by the simulation-sufficiency reading; they audit process adherence, not outcome validity. Their business model depends on the mandate's persistence.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, compliance_auditors, beneficiary,
    organized, biographical, mobile, global).

% Live downstream of high-hazard facilities (chemical plants, nuclear sites, aviation corridors, dams) whose operators hold simulation certificates. Bear the consequence when simulated competence fails to transfer to real catastrophe response. Cannot exit the risk zone; cannot audit the simulation; have no standing in the compliance process.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, public_at_risk, payer,
    powerless, generational, trapped, global).

% Execute emergency procedures trained only in simulators. Know the gap between scripted scenarios and the novelty, ambiguity, and physiological stakes of real events. Cannot refuse assignments on grounds of simulation-only preparation without career penalty. Their professional identity fuses with the simulation paradigm, making exit identity-locked for many.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_operators, payer,
    moderate, biographical, constrained, regional).

% Survivor communities, disaster investigators, and veterans of actual catastrophes who argue that simulation cannot replicate the judgment-under-stakes component of the competence kernel. Their testimony is heard in post-disaster inquiries but excluded from the standard-setting process that defines 'sufficient fidelity.'
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, lived_catastrophe_advocates, excluded,
    organized, biographical, constrained, global).

% Safety researchers and practitioners who argue the kernel has two components — procedural fluency (simulation-exercisable) and judgment-under-stakes (requires real or near-real activation). They are excluded from the regulatory definition of competence, which treats the kernel as unitary and simulation-satisfiable.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, hybrid_proponents, excluded,
    organized, biographical, constrained, global).

% Study transfer of training from simulation to real events. Produce evidence on fidelity thresholds, decay curves, and the judgment gap. Their findings are cited selectively by regulators (when supportive) and by excluded voices (when challenging). They hold no enforcement power.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, safety_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, repeatable, low-risk method for organizations to demonstrate procedural readiness for rare catastrophes, replacing the prior regime of unverifiable claims and ad-hoc drills with auditable simulation cycles.
% TRANSFER_FUNCTION: Transfers the burden of readiness verification from unpredictable lived catastrophe (unbounded cost, unbounded harm) to scheduled simulation budgets (bounded cost, zero harm during exercise). Transfers residual risk from regulated organizations to the public when simulation fidelity falls short of the competence kernel's full requirements. Transfers revenue to simulation vendors and auditors from organizational compliance budgets.
% ABSENT_VOICES: Communities that have experienced actual catastrophes and know the gap between drill and reality; frontline operators who cannot refuse assignments based on simulation-only credentials; insurers who price risk based on compliance certificates rather than demonstrated capability; the hybrid_decay_reading proponents who argue the kernel has a judgment component simulation cannot reach.
% DISAPPEARANCE_RATIONALE: If simulation mandates vanished overnight, the global regulatory architecture for high-hazard industries would lose its primary verification mechanism. Organizations would revert to experience-based readiness (waiting for catastrophe) or adopt unstructured internal standards. The simulation vendor ecosystem would collapse. The public would lose even the flawed assurance of auditable drills.
% FOUNDING_PROBLEM: After a sequence of major disasters (Bhopal 1984, Challenger 1986, Piper Alpha 1988, Deepwater Horizon 2010), investigations consistently found that organizations had no systematic way to verify catastrophe readiness without waiting for catastrophe itself. Simulation mandates were created to make preparedness auditable, routine, and non-catastrophic.
% FOUNDING_PROBLEM_CORROBORATION: Post-disaster commissions (independent) attest the original problem was real: organizations genuinely lacked verifiable readiness verification. Current safety researchers, survivor communities, and hybrid_decay_reading proponents attest the solution has become a substitute for the competence it was meant to verify — the mandate now certifies simulation performance, not catastrophe competence.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__simulation_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__simulation_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the gap between what organizations spend on simulation compliance and what real readiness would cost, plus the risk transfer to the public when fidelity proves insufficient. Suppression (0.65) is structural: regulatory safe-harbor rules make simulation compliance a legal shield, suppressing alternatives (hybrid models, live-exercise requirements, judgment-assessment methods). Theater ratio (0.42) is rising as the simulation vendor ecosystem optimizes for metric-passing scenarios rather than transfer validity. Accessibility collapse (0.55) is moderate — hybrid approaches exist but are not regulatory substitutes. Resistance (0.48) comes from excluded voices and post-disaster revelations but has not shifted the regulatory paradigm.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/agenda-setter seat, the constraint is a Rope: it solves the coordination problem of verifiable readiness without catastrophe. From the frontline operator and public seats, it computes as Tangled Rope or Snare: a genuine coordination function (procedural fluency) coexists with asymmetric extraction (judgment gap risk transferred to those with no voice). The engine computes per-seat classification from the declared roles, exit options, and power levels — the authored claim (tangled_rope) is the generating model's structural judgment, not a per-seat verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulators (agenda_setter) sit near d=0.15: they author the constraint, their legitimacy depends on it, they bear no downside. Regulated organizations and vendors (beneficiaries) sit near d=0.2: they capture cost avoidance and revenue, exit is constrained but not trapped. Frontline operators (payer) sit near d=0.75: identity-locked into the paradigm, bear the judgment gap personally, cannot exit without career loss. Public at risk (payer) sits at d=1.0: fully trapped, zero voice, bears the tail risk. The engine will compute this divergence from the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no verifiable readiness without catastrophe) was real and live. The simulation mandate solved it initially. But the mandate has drifted: it now certifies simulation performance as competence, while the judgment-under-stakes component of the kernel remains unexercised. The mandate persists because it serves the agenda-setter (auditable metrics), beneficiaries (cost avoidance, revenue), and the compliance ecosystem. The public and frontline operators pay the mandatrophy cost. The constraint is not a Piton — it is actively enforced and expanded — but its coordination function has narrowed while its extraction footprint has grown.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the simulation_sufficiency_reading a distinct constraint from its sibling readings, or a parameterization of a single constraint?',
    'Apply the epsilon-invariance test: if measuring the constraint via simulation-pass-rate yields low epsilon but measuring via post-catastrophe competence retention yields high epsilon, they are distinct constraints requiring separate stories. The kernel_id/reading_id structure asserts they are distinct.',
    'If distinct, each reading gets its own epsilon, stakeholders, and classification. The lived_catastrophe_necessity_reading would have higher extractiveness (entire competence unexercised) and different victims (all downstream of unexercised judgment). The hybrid_decay_reading would have split beneficiary/victim structures per component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s contested readings instantiate one constraint with measurement variance or multiple epsilon-invariant constraints.').

omega_variable(
    fidelity_competence_transfer_gap,
    'Does simulation fidelity (as currently defined and measured) actually predict competence retention for the judgment-under-stakes component of the kernel?',
    'Longitudinal studies comparing operator performance in real events vs. their simulation fidelity scores; natural experiments from disasters where operators had high simulation scores but poor real-world judgment.',
    'If fidelity does not predict judgment retention, the simulation_sufficiency_reading''s core axiom is empirically false — the constraint extracts via a false adequacy claim. If fidelity does predict, the reading''s claimed_type may shift toward rope (genuine coordination with manageable overhead).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_competence_transfer_gap, empirical, 'Whether the reading''s foundational empirical claim (fidelity determines retention) holds for the full competence kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative readiness models (hybrid, live-exercise, judgment-assessment) structural (regulatory safe-harbor rules, liability regimes) or internalized (operators and publics believe simulation certificates equal readiness)?',
    'Track post-exit suppression: if an organization adopts hybrid readiness and regulators still require simulation compliance, suppression is structural. If operators who know the gap still treat simulation-pass as ''ready'' cognitively, internalized component exists.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — the target carries the suppression cognitively. This would increase the constraint''s computed extraction for identity-locked agents (frontline operators).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the simulation-sufficiency paradigm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exercise_competence_sim_suff_tr_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(exercise_competence_sim_suff_tr_t5, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(exercise_competence_sim_suff_tr_t10, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(exercise_competence_sim_suff_tr_t15, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(exercise_competence_sim_suff_tr_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(exercise_competence_sim_suff_tr_t25, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(exercise_competence_sim_suff_tr_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(exercise_competence_sim_suff_tr_t35, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 35, 0.42).

% Extraction over time
narrative_ontology:measurement(exercise_competence_sim_suff_be_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(exercise_competence_sim_suff_be_t5, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(exercise_competence_sim_suff_be_t10, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(exercise_competence_sim_suff_be_t15, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(exercise_competence_sim_suff_be_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(exercise_competence_sim_suff_be_t25, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement(exercise_competence_sim_suff_be_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(exercise_competence_sim_suff_be_t35, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 35, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(exercise_competence_sim_suff_su_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(exercise_competence_sim_suff_su_t5, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(exercise_competence_sim_suff_su_t10, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(exercise_competence_sim_suff_su_t15, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(exercise_competence_sim_suff_su_t20, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(exercise_competence_sim_suff_su_t25, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 25, 0.64).
narrative_ontology:measurement(exercise_competence_sim_suff_su_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(exercise_competence_sim_suff_su_t35, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 35, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__simulation_sufficiency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.12).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial 'simulation-based preparedness' into three epsilon-invariant readings. The simulation_sufficiency_reading claims unitary kernel exercised by simulation (epsilon ~0.58). The lived_catastrophe_necessity_reading claims kernel only exercised by real stakes (epsilon ~0.8+ for the unexercised judgment component). The hybrid_decay_reading claims a two-component kernel with split exercise requirements (epsilon varies by component). They are linked because the simulation_sufficiency_reading's regulatory dominance suppresses the other two readings' adoption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, organized, 0.25).
constraint_indexing:directionality_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
