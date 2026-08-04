% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__real_incident_necessity, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Real-Incident Necessity Reading of the Competence Kernel
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   High-reliability organizations (nuclear operations, aviation, surgical
 *   teams, chemical process control) face a persistent anxiety: however
 *   extensive their drills and simulations, no one can be certain operators
 *   will perform correctly under an authentic catastrophic event, because
 *   catastrophic events have structural features — genuine irreversibility,
 *   genuine unbounded stakes, genuine physiological stress response — that no
 *   simulation, however high-fidelity, replicates. The
 *   real_incident_necessity reading takes this anxiety and elevates it to a
 *   structural claim: authentic occupation of the competence kernel requires
 *   the real thing. This creates an organizationally unresolvable problem,
 *   since the mechanism the reading names as necessary is also the mechanism
 *   every safety program exists to prevent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.31).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.22).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.31).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real-Incident Necessity Reading of the Competence Kernel").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "high_reliability_organizations/safety_training").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, 'd190c857-ddeb-457f-ac1e-8dd811a0759d').
narrative_ontology:cs_kernel_codification('d190c857-ddeb-457f-ac1e-8dd811a0759d', distributed).
narrative_ontology:cs_authority_grounding('d190c857-ddeb-457f-ac1e-8dd811a0759d', distributed).
narrative_ontology:cs_reading_relation('d190c857-ddeb-457f-ac1e-8dd811a0759d', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('d190c857-ddeb-457f-ac1e-8dd811a0759d', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('d190c857-ddeb-457f-ac1e-8dd811a0759d', foundational, authenticity_requires_irreversible_stakes).
narrative_ontology:cs_axiom_status(authenticity_requires_irreversible_stakes, holdable).
narrative_ontology:cs_axiom_grounding('d190c857-ddeb-457f-ac1e-8dd811a0759d', authenticity_requires_irreversible_stakes, empirically_contingent).
narrative_ontology:cs_axiom('d190c857-ddeb-457f-ac1e-8dd811a0759d', secondary, simulated_stress_response_is_categorically_distinct_from_genuine_stress_response).
narrative_ontology:cs_axiom_status(simulated_stress_response_is_categorically_distinct_from_genuine_stress_response, holdable).
narrative_ontology:cs_axiom_grounding('d190c857-ddeb-457f-ac1e-8dd811a0759d', simulated_stress_response_is_categorically_distinct_from_genuine_stress_response, empirically_contingent).
narrative_ontology:cs_reference_frame('d190c857-ddeb-457f-ac1e-8dd811a0759d', authentic_stress_calibration_standard).
narrative_ontology:cs_drift_state('d190c857-ddeb-457f-ac1e-8dd811a0759d', post_normal_accident_theory_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d190c857-ddeb-457f-ac1e-8dd811a0759d', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, incident_investigation_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, organizational_leadership).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, normal_accident_theory).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, authentic_exposure_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Work reactor control rooms, flight decks, or surgical theaters where the claim holds that their competence at handling true catastrophe can only be authentically calibrated by having lived through one. They cannot manufacture the incident that would certify them; they can only wait, drill, and hope the real test never arrives, knowing that if this reading is correct their preparedness is permanently unverifiable.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, frontline_operators, payer,
    moderate, biographical, trapped, local).

% Post-incident review boards (NTSB-style, INPO-style) derive their entire evidentiary base and institutional authority from the fact that real incidents are the only data source dense enough to reveal true operator competence under authentic stress. Every catastrophe that occurs increases the material they have to work with and the legitimacy of their methodology; they benefit epistemically from the reading without wanting the incidents themselves.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, incident_investigation_bodies, beneficiary,
    institutional, generational, analytical, national).

% Sets training budgets and certification regimes under a structural bind: if the real-incident-necessity reading is correct, no amount of simulation investment can close the competence-verification gap, so leadership pays continuously for drills they cannot prove are sufficient, while regulators and boards ask for assurances the reading says cannot honestly be given.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, organizational_leadership, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__real_incident_necessity, organizational_leadership, payer).

% Certify operators and facilities against competence standards while structurally unable to observe the one condition (authentic catastrophe) that this reading holds necessary. They administer proxies — simulation scores, audit findings — while this reading asserts those proxies cannot occupy the kernel, leaving certification itself epistemically hollow from inside this framework.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, regulators_and_licensing_boards, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__real_incident_necessity, regulators_and_licensing_boards, agenda_setter).

% Communities living near nuclear plants, chemical facilities, or under flight paths bear the downstream risk of any competence gap but have no voice in how competence is defined or verified; if the real-incident-necessity reading is correct, their safety guarantee is structurally unfalsifiable until the incident that would prove or disprove it actually harms them.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, affected_public, excluded,
    powerless, generational, trapped, regional).

% Builds and sells high-fidelity simulators and drill programs whose entire value proposition this reading structurally denies — if only real incidents occupy the kernel, simulation is definitionally insufficient no matter how well engineered, undercutting the industry's claim to solve the competence problem.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, simulation_and_training_industry, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__real_incident_necessity, diffuse).
narrative_ontology:fixing_cost_class(competence_occupation__real_incident_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the ordinary sense — this reading does not coordinate an achievable practice. It functions instead as an epistemic standard: it names what would count as authentic competence-occupation, which disciplines training design by ruling out claims of sufficiency for any non-catastrophic mechanism.
% TRANSFER_FUNCTION: No resources are transferred in ordinary operation. What is 'transferred' is epistemic legitimacy: real incidents, when they occur, transfer authority and evidentiary weight to investigation bodies and away from simulation-based certification regimes, at the cost of the harm the incident itself caused.
% ABSENT_VOICES: The affected public bears the risk of an unresolved competence-verification problem but has no seat in setting training or certification standards. The simulation industry is structurally excluded from the reading's own terms — no amount of engineering improvement lets them satisfy the standard this reading sets.
% DISAPPEARANCE_RATIONALE: If this reading vanished, high-reliability organizations would lose the framing that says catastrophe is the only authentic certifier — some (simulation vendors, training officers) would experience relief and reallocate confidence to drills; others (investigation-body epistemology, certain safety cultures built on 'we have not really been tested') would lose their grounding claim that current competence is unverified. Whether the world 'rearranges' or 'stays the same' depends on whether the organization's actual practice already quietly assumed the hybrid reading regardless of what this reading claims.
% FOUNDING_PROBLEM: High-reliability organizations needed a way to explain why near-misses and drills, however numerous, never fully settle anxiety about whether operators can perform under true catastrophic stress — the reading answers that anxiety by declaring the gap structurally unclosable short of a real event.
% FOUNDING_PROBLEM_CORROBORATION: Independent human-factors researchers studying post-incident performance (e.g. aviation CRM studies, nuclear near-miss literature) corroborate that operator behavior under confirmed real catastrophe sometimes diverges from simulator performance in ways not fully predicted beforehand — this is attested by researchers outside the investigation bodies and outside the organizations whose operators are being assessed, though the same researchers are divided on whether this establishes necessity or merely insufficiency of simulation.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, contested).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.31, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, ExtMetricName, E),
    domain_priors:suppression_score(competence_occupation__real_incident_necessity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(competence_occupation__real_incident_necessity),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(competence_occupation__real_incident_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.31) and rising only slightly across the interval: this reading does not itself extract resources from a captive population in the way a snare or tangled rope would — there is no active mechanism forcing anyone to produce real incidents to satisfy the standard. What extraction exists is diffuse and epistemic: organizations continue funding training programs whose sufficiency this reading denies in principle, and investigation bodies accrue authority each time a real incident does occur, without having engineered the incident. Suppression is low (0.22) because nothing coercively suppresses alternative readings — simulation_sufficiency and hybrid_occupation remain live, publicly argued positions; this reading does not need to silence its rivals to persist. Accessibility_collapse (0.62) and resistance (0.71) are set as befits an unresolved conceptual claim rather than a genuine physical law: alternatives are not fully collapsed (organizations can and do adopt hybrid or simulation-forward postures instead), and resistance is substantial because training officers, simulation vendors, and much of the human-factors research community actively contest the claim that real incidents are necessary rather than merely epistemically privileged.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontline operators bear the cost of an unresolvable verification standard without any offsetting benefit — their preparedness can never be authentically certified short of living through the catastrophe the whole safety apparatus exists to avoid, so they sit near the target end of directionality despite not being named formal 'victims' (no group is harmed BY this reading in the extraction sense; the harm is epistemic uncertainty, not resource transfer). Incident investigation bodies are the nearest thing to a beneficiary: their institutional authority and evidentiary richness increase with each real incident, even though they neither cause incidents nor want them — this is why the beneficiary declaration is narrow and heavily qualified in commentary rather than treated as an ordinary extractive beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a strong candidate for a false-summit-like structural pattern despite being claimed as mountain: the reading presents itself as a natural epistemic limit (only the real thing can authentically test the real thing) but names an institutional beneficiary (investigation bodies) whose authority is not fixed by natural law but by which epistemic standard organizations choose to adopt. Declaring the beneficiary on a mountain claim is intentional FSM authoring here — the omega below documents the natural-limit-vs-constructed-standard ambiguity the schema requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_epistemic_limit_vs_constructed_standard,
    'Is the real-incident-necessity claim a genuine feature of how competence-under-catastrophe can be known (a natural epistemic limit, like measurement uncertainty), or is it a constructed standard that certain institutions (investigation bodies, certain safety cultures) have an interest in maintaining because it grants their post-incident findings unique evidentiary authority?',
    'Compare organizations that formally adopt hybrid_occupation or simulation_sufficiency standards against those that implicitly operate under real_incident_necessity: if safety outcomes and actual operator performance under real incidents do not differ measurably by which standard the organization endorses, the necessity claim looks more constructed than natural.',
    'If constructed, the claimed mountain status is a false summit and the constraint reclassifies toward tangled_rope, with investigation bodies as a concentrated beneficiary of an artificially elevated evidentiary standard; if genuinely natural, the mountain claim holds and the beneficiary relationship is incidental rather than extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_epistemic_limit_vs_constructed_standard, conceptual, 'Whether real-incident-necessity is natural epistemic law or a constructed standard benefiting investigation authority.').

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one reading of the competence_occupation kernel (siblings: simulation_sufficiency, hybrid_occupation). Where exactly does the disagreement between readings live — is it about what evidence COUNTS as occupying the kernel, or about whether the kernel itself (a determinate, occupiable state of ''genuine competence'') exists at all?',
    'Structured elicitation from safety scientists and organizational leadership across all three reading-camps, isolating whether disputants agree a determinate competence-state exists and disagree only about evidentiary sufficiency, versus disagreeing about the kernel''s existence itself.',
    'If the disagreement is purely evidentiary, hybrid_occupation is likely the eventual convergence point (multi-mechanism as best-available evidence); if the disagreement is about the kernel''s existence, real_incident_necessity and simulation_sufficiency are not really competing readings of the same claim but expressions of a deeper unresolved question about whether ''competence under catastrophe'' is even a well-formed property.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the sibling-reading disagreement in evidentiary sufficiency versus kernel existence.').

omega_variable(
    unfalsifiability_of_negative_claim,
    'Because catastrophic incidents are rare and organizations actively work to prevent them, how would the real_incident_necessity claim ever be disconfirmed, short of catastrophes occurring and operators failing despite extensive simulation training?',
    'Track long-run safety records of organizations with heavy simulation investment against the rate and severity of real-incident performance failures; a persistent divergence would support the necessity claim, while convergence would undermine it — but base rates of catastrophe make statistical resolution extremely slow.',
    'If unfalsifiable in practice, the reading functions more as an institutional posture than an empirically testable structural claim, weakening its mountain candidacy regardless of the FSM resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unfalsifiability_of_negative_claim, empirical, 'The practical unfalsifiability of the necessity claim given catastrophe rarity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__real_incident_necessity, theater_ratio, 8, 0.13).
narrative_ontology:measurement_basis(comp_tr_t8, observed).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__real_incident_necessity, theater_ratio, 16, 0.15).
narrative_ontology:measurement_basis(comp_tr_t16, observed).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__real_incident_necessity, theater_ratio, 24, 0.16).
narrative_ontology:measurement_basis(comp_tr_t24, observed).
narrative_ontology:measurement(comp_tr_t32, competence_occupation__real_incident_necessity, theater_ratio, 32, 0.17).
narrative_ontology:measurement_basis(comp_tr_t32, projected).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__real_incident_necessity, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(comp_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.24).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t8, competence_occupation__real_incident_necessity, base_extractiveness, 8, 0.26).
narrative_ontology:measurement_basis(comp_be_t8, observed).
narrative_ontology:measurement(comp_be_t16, competence_occupation__real_incident_necessity, base_extractiveness, 16, 0.28).
narrative_ontology:measurement_basis(comp_be_t16, observed).
narrative_ontology:measurement(comp_be_t24, competence_occupation__real_incident_necessity, base_extractiveness, 24, 0.29).
narrative_ontology:measurement_basis(comp_be_t24, observed).
narrative_ontology:measurement(comp_be_t32, competence_occupation__real_incident_necessity, base_extractiveness, 32, 0.3).
narrative_ontology:measurement_basis(comp_be_t32, projected).
narrative_ontology:measurement(comp_be_t40, competence_occupation__real_incident_necessity, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(comp_be_t40, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(competence_occupation__real_incident_necessity, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_occupation__real_incident_necessity, 0.08).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the competence_occupation kernel. simulation_sufficiency claims drills adequately occupy the kernel (ε low, clear beneficiary structure in the simulation/training industry, likely rope or tangled_rope). hybrid_occupation claims no single mechanism suffices and requires continuous multi-mechanism exercise without configuration consensus (moderate ε, diffuse coordination costs, likely scaffold or tangled_rope given the unresolved configuration problem). real_incident_necessity (this story) claims only actual catastrophe suffices, producing the most severe epistemic bind and the thinnest beneficiary structure of the three, since no one can coherently want more catastrophes. All three readings are linked bidirectionally because organizational safety policy debates cite each reading against the others; a shift in institutional consensus toward one reading structurally pressures resource allocation away from the mechanisms the other readings privilege.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
