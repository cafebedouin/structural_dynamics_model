% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__catastrophe_necessity_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
 *   human_readable: Catastrophe Necessity for Genuine Operational Competence
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates the catastrophe_necessity_reading of the
 *   catastrophe_proxy_sufficiency kernel. It asserts that only actual
 *   catastrophic events provide an irreducible quality of stress and
 *   uncertainty necessary to maintain genuine operational competence, and
 *   that simulation is categorically insufficient to replicate this. The
 *   constraint is authored as a Mountain because it posits a physical and
 *   psychological limitâa natural boundary condition on learning under
 *   existential stakes. Operational safety margins erode in catastrophe-free
 *   periods as a consequence of this limit, but this erosion is a natural
 *   effect rather than an extractive relationship between parties. No
 *   beneficiaries or victims are declared because the constraint shapes all
 *   agents uniformly through natural law rather than through engineered
 *   extraction.
 *
 * KEY AGENTS:
 *   - high_reliability_operators: Frontline personnel in safety-critical systems who experience competence decay when operating exclusively in catastrophe-free, simulation-heavy regimes.
 *   - safety_engineering_experts: Analytical seat asserting the irreducibility claim from research on stress physiology and organizational learning.
 *   - simulation_technology_vendors: Excluded commercial voice with structural interest in rejecting irreducibility.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.12).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.05).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, mountain).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "Catastrophe Necessity for Genuine Operational Competence").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "safety_engineering/organizational_learning").

domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, '9f68c421-08c3-4f93-b873-138569597a46').
narrative_ontology:cs_kernel_codification('9f68c421-08c3-4f93-b873-138569597a46', distributed).
narrative_ontology:cs_authority_grounding('9f68c421-08c3-4f93-b873-138569597a46', expertise).
narrative_ontology:cs_interpretation_layer_present('9f68c421-08c3-4f93-b873-138569597a46').
narrative_ontology:cs_reading_relation('9f68c421-08c3-4f93-b873-138569597a46', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('9f68c421-08c3-4f93-b873-138569597a46', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f68c421-08c3-4f93-b873-138569597a46', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, forecloses).
narrative_ontology:cs_axiom('9f68c421-08c3-4f93-b873-138569597a46', foundational, irreducible_catastrophe_stress_hypothesis).
narrative_ontology:cs_axiom_status(irreducible_catastrophe_stress_hypothesis, holdable).
narrative_ontology:cs_axiom_grounding('9f68c421-08c3-4f93-b873-138569597a46', irreducible_catastrophe_stress_hypothesis, empirically_contingent).
narrative_ontology:cs_axiom('9f68c421-08c3-4f93-b873-138569597a46', foundational, genuine_competence_requires_existential_risk).
narrative_ontology:cs_axiom_status(genuine_competence_requires_existential_risk, holdable).
narrative_ontology:cs_axiom_grounding('9f68c421-08c3-4f93-b873-138569597a46', genuine_competence_requires_existential_risk, instrumental).
narrative_ontology:cs_reference_frame('9f68c421-08c3-4f93-b873-138569597a46', catastrophe_stress_baseline).
narrative_ontology:cs_drift_state('9f68c421-08c3-4f93-b873-138569597a46', high_fidelity_simulation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9f68c421-08c3-4f93-b873-138569597a46', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Does not coordinate human action; it describes a natural boundary condition under which all safety coordination must occur. It implicitly organizes high-reliability practice around the necessity of genuine high-stakes experience.
% TRANSFER_FUNCTION: No agent-to-agent transfer function. The constraint imposes a diffuse natural costâcompetence decay and erosion of operational safety margins during catastrophe-free periodsâwithout moving resources between parties.
% ABSENT_VOICES: Simulation technology vendors and commercial training providers, who have a structural interest in asserting simulation sufficiency and are largely excluded from high-reliability-organization theory discourse; also, operators who have successfully relied on simulation without catastrophe exposure and dispute the decay claim.
% DISAPPEARANCE_RATIONALE: The constraint describes a natural limit, not a human arrangement. If it vanishedâif simulation became sufficient for genuine competenceâhuman organizations would simply operate under a changed natural reality. No institutional rearrangement is required because no institution is organized around extracting value from the constraint.
% FOUNDING_PROBLEM: How to maintain genuine operational competence in safety-critical domains where catastrophic failures are rare but high-consequence, and rehearsal without existential stakes may produce only performative readiness.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards repeatedly identify competence decay and normalization of deviance in catastrophe-free periods as contributors to failure. High-reliability organization theorists attest to the problem from an analytical seat outside commercial simulation interests.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, world_unchanged).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.12, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.12 to reflect that while the constraint imposes a real costâcompetence decay and erosion of operational safety margins during catastrophe-free periodsâit is not an engineered extraction mechanism but a natural limit. Suppression is negligible (0.05) because the constraint requires no active enforcement; it describes a boundary that organizations encounter regardless of institutional design. Theater ratio is near-zero (0.02) as there is no performative maintenance. Accessibility collapse is high (0.88) because once the irreducibility claim is accepted, the alternative of simulation-as-sufficient collapses as a viable path to genuine competence. Resistance is low (0.08) because the natural limit itself is not actively resisted, though its policy implications are contested across the kernel's sibling readings. Measurements are flat across the interval because a Mountain does not drift.
 *
 * PERSPECTIVAL GAP:
 *   Not applicable within this reading: the constraint is a Mountain with no seated parties. All agents occupying the same spatial and power context face the same natural limit. Divergence between seats appears only across the kernel's sibling readings, not within this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality derivation applies. The constraint has no beneficiaries or victims in the extractive sense. The cost flows to operational safety margins as a diffuse system property, not as a targeted agent. Directionality for all practical seats is symmetric because the natural law applies uniformly.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable. The constraint has no mandate and no institutional maintenance requirement; it persists independent of any human enforcement or administrative performance. The Mountain classification prevents mislabeling the erosion of safety margins as extraction by an agenda-setter.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_irreducibility_empirical_status,
    'Is the irreducibility of real-catastrophe stress a genuine physical/psychological limit, or a contingent technological boundary that advancing simulation technology could eventually cross?',
    'High-fidelity neurophysiological and field studies comparing stress markers, decision latency, and team coordination in real versus high-fidelity simulated catastrophes; longitudinal tracking of operational outcomes across simulation-only cohorts.',
    'If reducible, this Mountain reclassifies to a contested empirical claim and potentially to a Scaffold or Snare depending on who enforces simulation-only training regimes. If irreducible, the Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_irreducibility_empirical_status, empirical, 'Whether irreducibility is a natural law or contingent limit').

omega_variable(
    genuine_competence_normative_status,
    'Does genuine competence in this reading name a measurable operational construct, or a normative ideal that resists falsification and functions as an identity marker for experienced operators?',
    'Operationalization of competence into independently measurable componentsâprocedural accuracy, tacit pattern recognition, stress-response latency, team coordinationâand validation against incident and near-miss outcomes.',
    'If purely normative, the constraint''s empirical status weakens and it drifts toward identity_coordination, enforcing a professional identity around real experience rather than describing a natural limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_competence_normative_status, conceptual, 'Whether genuine competence is measurable or normative').

omega_variable(
    kernel_reading_decomposition,
    'This constraint is one reading of kernel catastrophe_proxy_sufficiency. Sibling readings include simulation_as_proxy_catastrophe_reading (sufficiency), hybrid_degradation_reading (partial/generational), and simulation_fidelity_threshold (technology-dependent). Does the kernel decompose into structurally distinct constraints per epsilon-invariance, or do the readings converge on a single empirically resolvable claim?',
    'Empirical resolution of whether stress-response physiology is categorically non-replicable by simulation, and whether competence metrics show threshold effects at specific fidelity levels.',
    'If the readings are empirically distinct, they remain separate constraints linked in a family; if they converge, the family collapses to a single constraint with measurement-dependent classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Structural ambiguity across kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(cata_tr_t15, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 15, 0.02).
narrative_ontology:measurement(cata_tr_t30, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 30, 0.02).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cata_be_t15, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 15, 0.12).
narrative_ontology:measurement(cata_be_t30, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 30, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% The catastrophe_proxy_sufficiency kernel decomposes into four epsilon-invariant constraints: catastrophe_necessity_reading (Mountain, irreducible limit), simulation_as_proxy_catastrophe_reading (claims sufficiency), hybrid_degradation_reading (claims partial/generational decay), and simulation_fidelity_threshold (claims technology-dependent reducibility). They are linked as a constraint family because they share a regulatory and pedagogical domain but posit different structural relationships between simulation and competence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
