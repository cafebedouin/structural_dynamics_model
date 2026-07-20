% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__simulation_as_sufficient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__simulation_as_sufficient, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: competence_retention_exercise__simulation_as_sufficient
 *   human_readable: High-Fidelity Simulation as Sufficient Competence Exercise
 *   domain: safety engineering/organizational learning
 *
 * SUMMARY:
 *   Within high-reliability organizations, an institutionalized claim holds
 *   that high-fidelity simulation is structurally equivalent to real
 *   catastrophic events for maintaining operator competence. This claim
 *   underpins certification standards, training budgets, and licensing
 *   regimes that replace or marginalize experiential learning pathways. The
 *   constraint coordinates distributed readiness without requiring
 *   catastrophes, but it also channels resources toward simulation
 *   infrastructure, locks operator identity to simulator metrics, and
 *   suppresses dissent from experienced practitioners who argue that
 *   unscripted stakes are irreplaceable. This story instantiates the
 *   simulation_as_sufficient reading of the competence_retention_exercise
 *   kernel.
 *
 * KEY AGENTS:
 *   - simulation_vendors: Primary beneficiary (powerful/mobile) â commercial capturer of training infrastructure budgets and upgrade cycles.
 *   - certification_authorities: Agenda setter (institutional/analytical) â codifies and enforces simulator equivalence as a formal standard.
 *   - control_room_operators: Primary target (moderate/identity_locked) â assessed by simulator metrics, bear transfer risk, and lack authority to challenge the equivalence regime.
 *   - risk_bearing_communities: Secondary target (powerless/trapped) â bear catastrophic downside if simulated competence decays, with no voice in training design.
 *   - experienced_operators: Excluded voice (moderate/constrained) â marginalized advocates for experiential learning whose firsthand knowledge is treated as anecdotal.
 *   - independent_safety_researchers: Analytical observer (organized/analytical) â investigate transfer validity but are excluded from standard-setting uptake.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, 0.75).
domain_priors:suppression_score(competence_retention_exercise__simulation_as_sufficient, 0.8).
domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, extractiveness, 0.75).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__simulation_as_sufficient, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__simulation_as_sufficient, "High-Fidelity Simulation as Sufficient Competence Exercise").
narrative_ontology:topic_domain(competence_retention_exercise__simulation_as_sufficient, "safety engineering/organizational learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__simulation_as_sufficient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__simulation_as_sufficient, '98808f50-9be7-48e1-ab02-5308cf4b3ea1').
narrative_ontology:cs_kernel_codification('98808f50-9be7-48e1-ab02-5308cf4b3ea1', formalized).
narrative_ontology:cs_authority_grounding('98808f50-9be7-48e1-ab02-5308cf4b3ea1', expertise).
narrative_ontology:cs_interpretation_layer_present('98808f50-9be7-48e1-ab02-5308cf4b3ea1').
narrative_ontology:cs_reading_relation('98808f50-9be7-48e1-ab02-5308cf4b3ea1', competence_retention_exercise__catastrophe_as_necessary, forecloses).
narrative_ontology:cs_reading_relation('98808f50-9be7-48e1-ab02-5308cf4b3ea1', competence_retention_exercise__near_miss_as_bridge, influences).
narrative_ontology:cs_axiom('98808f50-9be7-48e1-ab02-5308cf4b3ea1', foundational, simulation_structural_equivalence).
narrative_ontology:cs_axiom_status(simulation_structural_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('98808f50-9be7-48e1-ab02-5308cf4b3ea1', simulation_structural_equivalence, empirically_contingent).
narrative_ontology:cs_axiom('98808f50-9be7-48e1-ab02-5308cf4b3ea1', secondary, performance_metric_proximity).
narrative_ontology:cs_axiom_status(performance_metric_proximity, holdable).
narrative_ontology:cs_axiom_grounding('98808f50-9be7-48e1-ab02-5308cf4b3ea1', performance_metric_proximity, empirically_contingent).
narrative_ontology:cs_reference_frame('98808f50-9be7-48e1-ab02-5308cf4b3ea1', empirical_equivalence_framework).
narrative_ontology:cs_drift_state('98808f50-9be7-48e1-ab02-5308cf4b3ea1', post_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('98808f50-9be7-48e1-ab02-5308cf4b3ea1', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, simulation_vendors).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, hro_management).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, control_room_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, risk_bearing_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, sell, and maintain high-fidelity simulation systems to high-reliability organizations. Revenue and growth depend on institutional acceptance that simulator hours constitute genuine exercise of catastrophe-avoidance competence. They market equivalence claims, upgrade cycles, and fidelity improvements to training departments and certification bodies.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, simulation_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Set licensing and recertification standards for high-risk industries. They codify simulator performance thresholds as equivalent to operational experience, mandate simulator hours, and audit training programs against these metrics. Their authority rests on expertise claims in human factors and learning science.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, certification_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Operate high-risk facilities under regulatory frameworks that accept simulator-based competence demonstration. They benefit from reduced liability exposure, predictable training schedules, and measurable compliance artifacts. They allocate capital toward approved simulation infrastructure and defend the equivalence claim in safety cases and regulatory filings.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, hro_management, beneficiary,
    institutional, biographical, constrained, national).

% Must demonstrate competence through prescribed simulator scenarios to maintain licenses and shift assignments. Their professional identity and career progression are increasingly tied to simulator proficiency metrics. They bear the risk if simulator-trained responses fail to transfer to ambiguous, high-stakes real-world incidents, and they lack authority to challenge the equivalence claim without jeopardizing their standing.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, control_room_operators, payer,
    moderate, biographical, identity_locked, local).

% Veteran practitioners who acquired competence through actual near-misses, catastrophic events, or long unstructured apprenticeships. Their experiential knowledge is treated as anecdotal rather than systematic, and their advocacy for real-event learning is marginalized in training design committees and standard-setting bodies.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, experienced_operators, excluded,
    moderate, biographical, constrained, local).

% Study transfer validity from simulation to real operational performance, publishing findings on cognitive fidelity gaps and simulator limitations. Their work is often sequestered in academic venues rather than integrated into certification standards, and they do not collect rents from either the simulation industry or the training regimes they evaluate.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, independent_safety_researchers, observer,
    organized, generational, analytical, national).

% Live in proximity to high-risk facilities and bear catastrophic consequence if simulator-inflated competence fails during a real incident. They have no seat in training design, certification standards, or simulator procurement decisions, and they cannot opt out of the risk generated by the competence regime.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, risk_bearing_communities, payer,
    powerless, generational, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__simulation_as_sufficient, simulation_vendors).
narrative_ontology:fixing_cost_class(competence_retention_exercise__simulation_as_sufficient, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains catastrophe-avoidance competence across distributed high-risk organizations without requiring actual catastrophic events, providing a repeatable, scalable, and auditable training environment that keeps readiness high between rare incidents.
% TRANSFER_FUNCTION: Moves institutional resourcesâbudget, personnel time, and regulatory legitimacyâfrom experiential and apprenticeship-based learning regimes into simulator-based training infrastructure. Moves risk-assessment authority from frontline experiential judgment to simulator performance metrics, and transfers catastrophic downside risk to local communities while operators and management absorb compliance upside.
% ABSENT_VOICES: Experienced operators who argue that visceral stakes and unscripted ambiguity are irreplaceable; communities who bear catastrophic risk but are excluded from training design; dissenting safety researchers whose transfer-validity findings are not incorporated into standards; and catastrophe survivors whose firsthand knowledge is treated as trauma rather than data.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, certification regimes would need to rebuild experiential learning pipelines and apprenticeship networks that have atrophied, training budgets would reallocate away from simulator vendors, safety cases would need to be rewritten around non-simulated competence evidence, and the simulation industry's contracted market would reorganize around supplemental rather than replacement training.
% FOUNDING_PROBLEM: Catastrophic events in high-risk domains are too rare, destructive, and ethically unacceptable to serve as the primary training ground; organizations need a reliable mechanism to maintain operator readiness during long intervals between serious incidents.
% FOUNDING_PROBLEM_CORROBORATION: The simulation industry and HRO management attest the problem remains live, citing rarity of catastrophic events. Independent safety researchers and experienced operators attest that the founding problem has been partially solved by simulation but cannot be fully addressed without preserving real-world experiential feedback; empirical transfer-validity studies and incident-investigation reports from outside the benefiting parties support the partially-solved reading.
narrative_ontology:disappearance_verdict(competence_retention_exercise__simulation_as_sufficient, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__simulation_as_sufficient, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__simulation_as_sufficient, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__simulation_as_sufficient, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__simulation_as_sufficient, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__simulation_as_sufficient_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__simulation_as_sufficient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.30 to 0.75) as simulation shifts from a supplemental tool to the primary competence mechanism, decoupling certification from real-world performance. Suppression rises (0.25 to 0.80) as the institutional commitment hardens and dissenting experiential voices are structurally excluded from committees and funding. Theater rises (0.15 to 0.55) because compliance metricsâhours logged, scenarios passedâincreasingly substitute for genuine readiness verification. Accessibility_collapse (0.60) reflects the retirement of catastrophe-experienced mentors and the defunding of alternative training pipelines. Resistance (0.45) is moderate: organized researchers and some operators push back, but they are marginalized. The measurement series share one time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   From the certification authority and management seats, the constraint is efficient coordination that prevents catastrophes and provides auditable readiness. From the operator and community seats, the same structure enforces a competence metric that may not transfer, externalizes catastrophic risk, and extracts career compliance labor. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation vendors and HRO management are structural beneficiaries: they collect revenue or liability reduction from the equivalence claim, yielding low directionality. Certification authorities accrue authority and budget from administering the regime, sitting between beneficiary and symmetric. Control room operators are identity-locked targets: their professional standing is indexed to simulator performance, giving them high directionality. Risk-bearing communities are trapped targets with no exit, also yielding high directionality. Experienced operators are excluded from the derivation network but would register as high-target if admitted.
 *
 * MANDATROPHY ANALYSIS:
 *   Treating this as pure rope would miss the asymmetric extraction toward operators and communities and the suppression of experiential dissent. Treating it as pure snare would miss the genuine coordination value of maintaining distributed readiness without requiring actual catastrophes. The tangled_rope classification captures that the same structure coordinates industry-wide training while asymmetrically extracting from those who bear the competence risk and those who cannot exit the hazard zone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_equivalence_empirical_status,
    'Is high-fidelity simulation genuinely structurally equivalent to real catastrophic events in producing and maintaining operator competence, or is the equivalence claim an institutional construction that benefits training infrastructure interests?',
    'Controlled field studies comparing simulator-trained and catastrophe-experienced operators in live incident response; longitudinal analysis of accident rates in organizations that have shifted to simulator-only competence regimes.',
    'If the equivalence claim is empirically false, the constraint is more extractive than coordinated and the victim set expands significantly. If empirically true, the coordination function dominates and victim claims weaken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_equivalence_empirical_status, empirical, 'Empirical status of the structural equivalence claim between simulation and real events').

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the simulation_as_sufficient reading of the competence_retention_exercise kernel. How would the structural classification change if the catastrophe_as_necessary or near_miss_as_bridge readings were adopted instead?',
    'Comparative constraint-story analysis of the three readings as separate files with distinct beneficiary-victim structures, epsilon values, and directionality profiles.',
    'The sibling readings would redistribute directionality: catastrophe_as_necessary would treat catastrophic events as necessary learning investments rather than avoidable failures, reversing the victim-beneficiary polarity for affected communities; near_miss_as_bridge would moderate extraction by preserving a real-world validation layer and shrinking the extraction base.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Contested kernel reading ambiguity for competence retention exercise').

omega_variable(
    suppression_of_experiential_dissent,
    'Is the marginalization of experienced practitioners who question simulator validity driven by structural career penalties and funding exclusion, or by internalized professional identity shifts that accept simulation as legitimate competence currency?',
    'Career trajectory analysis of dissenting safety researchers and senior operators; ethnographic study of professional identity formation in simulator-centric training regimes.',
    'If structural, suppression is higher than measured and resistance is underreported. If internalized, the constraint''s effective hold persists even after structural barriers are removed because the agents carry the suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_experiential_dissent, empirical, 'Structural versus internalized suppression of dissenting experiential voices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__simulation_as_sufficient, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comp_tr_t8, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 8, 0.22).
narrative_ontology:measurement(comp_tr_t16, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 16, 0.3).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 24, 0.38).
narrative_ontology:measurement(comp_tr_t32, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 32, 0.48).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(comp_be_t8, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(comp_be_t16, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(comp_be_t32, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 40, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(comp_su_t8, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(comp_su_t16, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(comp_su_t24, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(comp_su_t32, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 40, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__simulation_as_sufficient, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_retention_exercise kernel, decomposed per the Îµ-invariance principle. The sibling readings instantiate structurally distinct claims with different epsilon values, beneficiary-victim structures, and directionality profiles. The kernel label 'competence retention exercise' conflates three separate constraints; the framework treats them as a constraint family linked by network edges, not as one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
