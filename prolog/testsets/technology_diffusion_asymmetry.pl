% ============================================================================
% CONSTRAINT STORY: technology_diffusion_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_diffusion_asymmetry, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technology_diffusion_asymmetry
 *   human_readable: Technology Diffusion Asymmetry: Consumer-Military Drone Capability Convergence
 *   domain: security_studies/technology_governance
 *
 * SUMMARY:
 *   The capability gap between consumer and military drones has compressed
 *   from orders of magnitude to near-parity over two decades. A 2005 military
 *   reconnaissance drone (Boeing ScanEagle: $100k, 10kg payload, 100km range)
 *   is now matched by 2025 consumer platforms (DJI Matrice: $1.5k, 6kg
 *   payload, 15km range with relay). The convergence is driven by
 *   semiconductor miniaturization (Moore's Law), battery energy density
 *   improvements (lithium chemistry), and manufacturing scale (Shenzhen
 *   electronics ecosystem). Export controls attempt suppression but cannot
 *   reverse the underlying physics and economics. The constraint is claimed
 *   as mountain because the diffusion trajectory is not maintained by any
 *   party — it is the emergent outcome of commercial technology development
 *   that no institution controls.
 *
 * KEY AGENTS:
 *   - non_state_actors: Primary beneficiary (moderate/mobile) — acquire military-relevant capability at consumer prices
 *   - insurgent_groups: Primary beneficiary (organized/mobile) — weaponize commercial platforms for asymmetric operations
 *   - consumer_drone_manufacturers: Institutional beneficiary (institutional/constrained) — profit from capability maximization; dual-use risk externalized
 *   - state_militaries: Primary payer (institutional/constrained) — lose procurement advantage as commercial R&D outpaces defense cycles
 *   - export_control_regimes: Agenda setter (institutional/constrained) — attempt regulatory suppression of diffusion that routes around control points
 *   - civilian_populations: Secondary payer (powerless/trapped) — bear security externalities with no control over diffusion
 *   - security_studies_analysts: Observer (analytical/analytical) — document trajectory and implications
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_diffusion_asymmetry, 0.12).
domain_priors:suppression_score(technology_diffusion_asymmetry, 0.08).
domain_priors:theater_ratio(technology_diffusion_asymmetry, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_diffusion_asymmetry, extractiveness, 0.12).
narrative_ontology:constraint_metric(technology_diffusion_asymmetry, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(technology_diffusion_asymmetry, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_diffusion_asymmetry, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(technology_diffusion_asymmetry, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_diffusion_asymmetry, mountain).
narrative_ontology:human_readable(technology_diffusion_asymmetry, "Technology Diffusion Asymmetry: Consumer-Military Drone Capability Convergence").
narrative_ontology:topic_domain(technology_diffusion_asymmetry, "security_studies/technology_governance").

domain_priors:emerges_naturally(technology_diffusion_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_diffusion_asymmetry, non_state_actors).
narrative_ontology:constraint_beneficiary(technology_diffusion_asymmetry, insurgent_groups).
narrative_ontology:constraint_beneficiary(technology_diffusion_asymmetry, consumer_drone_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_diffusion_asymmetry, state_militaries).
narrative_ontology:constraint_victim(technology_diffusion_asymmetry, civilian_populations).
narrative_ontology:constraint_vindicates(technology_diffusion_asymmetry, asymmetric_warfare_inevitability).
narrative_ontology:constraint_vindicates(technology_diffusion_asymmetry, dual_use_technology_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acquire near-military-grade aerial reconnaissance and strike capabilities at consumer prices. What cost a state military $100,000 per unit in 2005 now costs $1,500 at retail. They operate in regulatory gray zones where export controls cannot reach and adapt commercial platforms faster than defense procurement cycles can respond.
narrative_ontology:constraint_stakeholder(technology_diffusion_asymmetry, non_state_actors, beneficiary,
    moderate, biographical, mobile, global).

% Weaponize commercial drones for asymmetric operations: surveillance, improvised munitions delivery, swarm tactics. The capability gap that once required state sponsorship now requires only access to consumer electronics markets. Their tactical innovation cycle runs faster than institutional countermeasure development.
narrative_ontology:constraint_stakeholder(technology_diffusion_asymmetry, insurgent_groups, beneficiary,
    organized, biographical, mobile, regional).

% Develop and sell increasingly capable platforms driven by commercial competition: longer range, higher payload, better autonomy. They face export control pressure but the underlying technology trajectory is driven by Moore's Law and battery chemistry, not policy. Their market incentive is capability maximization; dual-use risk is externalized.
narrative_ontology:constraint_stakeholder(technology_diffusion_asymmetry, consumer_drone_manufacturers, beneficiary,
    institutional, biographical, constrained, global).

% Face adversaries equipped with capabilities that were exclusive military assets a decade prior. Their procurement advantage erodes as commercial R&D outpaces defense development cycles. They invest in counter-drone systems and attempt export controls, but the diffusion is driven by physics and economics, not policy compliance.
narrative_ontology:constraint_stakeholder(technology_diffusion_asymmetry, state_militaries, payer,
    institutional, generational, constrained, national).

% Attempt to regulate dual-use technology transfer through licensing and end-user verification. Their frameworks were designed for specialized military hardware with limited suppliers; consumer electronics diffusion operates through decentralized global supply chains that route around control points faster than treaties can adapt.
narrative_ontology:constraint_stakeholder(technology_diffusion_asymmetry, export_control_regimes, agenda_setter,
    institutional, generational, constrained, global).

% Bear the security externalities of weaponized consumer technology: surveillance, targeted strikes, infrastructure disruption. They have no control over the diffusion process and limited recourse when commercial platforms are repurposed for coercion or violence in their vicinity.
narrative_ontology:constraint_stakeholder(technology_diffusion_asymmetry, civilian_populations, payer,
    powerless, biographical, trapped, regional).

% Document the capability convergence trajectory and its strategic implications. They measure the parity index, model diffusion rates, and advise on countermeasures, but the underlying constraint is a function of semiconductor economics and battery energy density, not institutional choice.
narrative_ontology:constraint_stakeholder(technology_diffusion_asymmetry, security_studies_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this is not a coordination mechanism. It is a physical-economic constraint: the rate at which manufacturing cost curves and component performance improve, making capabilities that were once exclusive to state militaries accessible at consumer price points.
% TRANSFER_FUNCTION: Transfers military-relevant capability from exclusive state control to distributed non-state access. The transfer is not organized by any party; it is the emergent result of commercial technology development driven by consumer demand for photography, delivery, and hobbyist applications.
% ABSENT_VOICES: Civilian populations in conflict zones who bear the security costs of weaponized consumer technology have no seat in the commercial development process or export control negotiations. Their vulnerability is a negative externality of the diffusion trajectory.
% DISAPPEARANCE_RATIONALE: If the constraint 'disappeared' — meaning the diffusion trajectory reversed and consumer capabilities regressed — it would require reversing Moore's Law, battery chemistry progress, and global manufacturing integration. The world would not rearrange; the claim itself is physically incoherent. The constraint is a description of technological and economic reality, not an arrangement anyone maintains.
% FOUNDING_PROBLEM: Not applicable — this constraint was not 'built' to solve a problem. It is the observed outcome of semiconductor miniaturization, battery energy density improvements, and manufacturing cost reduction applied to aerial platforms. The diffusion is a side effect of consumer electronics development, not a designed solution.
% FOUNDING_PROBLEM_CORROBORATION: Security studies analysts and defense institutions attest that the diffusion is an unintended consequence of commercial innovation, not a coordinated transfer. Consumer drone manufacturers attest they are optimizing for commercial use cases, not military parity. The 'founding problem' framing is itself contested: there is no founding problem because there is no founded arrangement.
narrative_ontology:disappearance_verdict(technology_diffusion_asymmetry, world_unchanged).
narrative_ontology:founding_problem_status(technology_diffusion_asymmetry, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_diffusion_asymmetry, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-03',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(technology_diffusion_asymmetry, 'none', 1).
narrative_ontology:epsilon_provenance(technology_diffusion_asymmetry, 0.12, 'claude-sonnet-4-5-20250929', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_diffusion_asymmetry_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(technology_diffusion_asymmetry, ExtMetricName, E),
    domain_priors:suppression_score(technology_diffusion_asymmetry, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(technology_diffusion_asymmetry),
    narrative_ontology:constraint_metric(technology_diffusion_asymmetry, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(technology_diffusion_asymmetry, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(technology_diffusion_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the diffusion is not organized extraction — it is a side effect of commercial innovation. The modest extraction present reflects: (1) security externalities borne by civilian populations who gain no benefit, (2) defense procurement disadvantage as state militaries pay institutional overhead for capabilities available at retail, (3) regulatory compliance costs imposed on manufacturers by export control regimes. Suppression is very low (0.08) because export controls are structurally ineffective against decentralized supply chains and dual-use components. Theater is negligible (0.05) because the constraint operates through real physical and economic mechanisms, not performance. Accessibility collapse is very high (0.92) because once the capability parity is understood, no alternative trajectory exists — you cannot un-invent lithium batteries or reverse semiconductor scaling. Resistance is very low (0.03) because the diffusion is not a policy anyone defends; it is a physical-economic fact that institutions attempt to manage but cannot reverse. The measurement series shows extractiveness and suppression declining over the interval as the diffusion becomes more complete and export controls become less effective.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (non-state actors, manufacturers) and the payer seats (state militaries, civilian populations) experience radically different constraint types from the same underlying trajectory. From the non-state actor position, the diffusion operates as a capability windfall — a mountain that delivers military-relevant technology at consumer prices. From the state military position, the same trajectory operates as an erosion of strategic advantage that no amount of suppression can reverse. From the civilian population position, it operates as an imposed security threat. The engine computes these divergences from the structural data; the mountain claim reflects the constraint's physical-economic nature, not any seat's experience of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-state actors and insurgent groups are structural beneficiaries (d near 0.0-0.2): they gain capability without bearing development costs. Consumer drone manufacturers are beneficiaries (d ~0.15): they profit from capability maximization while externalizing dual-use risk. State militaries are targets (d ~0.6): they lose exclusive capability and must invest in countermeasures. Civilian populations are targets (d ~0.8): they bear security costs with no exit. Export control regimes sit near symmetric (d ~0.5): they attempt to manage the diffusion but the constraint operates independently of their enforcement. The directionality spread is wide because the same physical-economic trajectory benefits some actors and harms others, even though no party controls it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophy because its function has not outlived its justification — it never had a designed function. The diffusion is an emergent outcome of commercial technology development, not a coordinated arrangement. The 'mandate' framing does not apply: there is no institution whose authority depends on the constraint persisting, and no founding problem the constraint was built to solve. The modest extraction present is a negative externality of the diffusion, not rents collected by a beneficiary maintaining the arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_institutional_artifact,
    'Is the technology diffusion trajectory a natural law of semiconductor economics and battery chemistry, or is it an institutional artifact sustained by specific policy choices (patent regimes, export control enforcement levels, R&D subsidy structures)?',
    'Counterfactual analysis: if export controls were perfectly enforced or if dual-use R&D were prohibited, would the diffusion trajectory reverse or merely slow? If it would reverse, the constraint is institutional; if it would only slow, the constraint is closer to natural law with institutional modulation.',
    'If institutional, the constraint is a tangled rope (coordination of commercial innovation + extraction via security externalities) rather than a mountain. If natural law with institutional modulation, the mountain classification holds but the beneficiary presence indicates a false summit — the diffusion is inevitable but specific actors capture disproportionate benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_artifact, conceptual, 'Whether the diffusion trajectory is a physical-economic inevitability or an institutionally sustained outcome.').

omega_variable(
    capability_parity_threshold,
    'At what capability parity index does the diffusion asymmetry cease to be a meaningful constraint? Is it when consumer platforms match 50% of military specs, 80%, or 100%?',
    'Empirical observation of tactical substitution: when do non-state actors stop seeking military-grade platforms and rely exclusively on modified consumer hardware? That substitution point reveals the effective parity threshold.',
    'If the threshold is already crossed (consumer platforms are ''good enough'' for most asymmetric operations), the constraint''s active phase is complete and what remains is institutional adjustment to a new equilibrium. If the threshold is not yet crossed, the diffusion is still accumulating strategic impact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capability_parity_threshold, empirical, 'What level of capability convergence constitutes operationally meaningful parity.').

omega_variable(
    beneficiary_capture_vs_windfall,
    'Do the identified beneficiaries (non-state actors, manufacturers) actively shape the diffusion trajectory to maximize their benefit, or are they passive recipients of a trajectory driven by broader commercial forces?',
    'Analysis of manufacturer R&D priorities and non-state actor feedback loops into commercial development. If manufacturers optimize for dual-use applications or if non-state tactical innovation feeds back into commercial feature sets, beneficiaries are shaping the constraint. If not, they are windfall recipients.',
    'If beneficiaries actively shape the trajectory, the mountain classification is incorrect — the constraint is a tangled rope with coordination (commercial innovation) and extraction (security externalities) components. If beneficiaries are passive, the mountain classification holds and the beneficiary presence indicates a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_windfall, empirical, 'Whether beneficiaries are active shapers or passive recipients of the diffusion trajectory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_diffusion_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_diffusion_asymmetry, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(tech_tr_t0, observed).
narrative_ontology:measurement(tech_tr_t4, technology_diffusion_asymmetry, theater_ratio, 4, 0.07).
narrative_ontology:measurement_basis(tech_tr_t4, observed).
narrative_ontology:measurement(tech_tr_t8, technology_diffusion_asymmetry, theater_ratio, 8, 0.06).
narrative_ontology:measurement_basis(tech_tr_t8, observed).
narrative_ontology:measurement(tech_tr_t12, technology_diffusion_asymmetry, theater_ratio, 12, 0.05).
narrative_ontology:measurement_basis(tech_tr_t12, observed).
narrative_ontology:measurement(tech_tr_t16, technology_diffusion_asymmetry, theater_ratio, 16, 0.05).
narrative_ontology:measurement_basis(tech_tr_t16, observed).
narrative_ontology:measurement(tech_tr_t20, technology_diffusion_asymmetry, theater_ratio, 20, 0.05).
narrative_ontology:measurement_basis(tech_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_diffusion_asymmetry, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(tech_be_t0, observed).
narrative_ontology:measurement(tech_be_t4, technology_diffusion_asymmetry, base_extractiveness, 4, 0.16).
narrative_ontology:measurement_basis(tech_be_t4, observed).
narrative_ontology:measurement(tech_be_t8, technology_diffusion_asymmetry, base_extractiveness, 8, 0.14).
narrative_ontology:measurement_basis(tech_be_t8, observed).
narrative_ontology:measurement(tech_be_t12, technology_diffusion_asymmetry, base_extractiveness, 12, 0.13).
narrative_ontology:measurement_basis(tech_be_t12, observed).
narrative_ontology:measurement(tech_be_t16, technology_diffusion_asymmetry, base_extractiveness, 16, 0.12).
narrative_ontology:measurement_basis(tech_be_t16, observed).
narrative_ontology:measurement(tech_be_t20, technology_diffusion_asymmetry, base_extractiveness, 20, 0.12).
narrative_ontology:measurement_basis(tech_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_diffusion_asymmetry, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(tech_su_t0, observed).
narrative_ontology:measurement(tech_su_t4, technology_diffusion_asymmetry, suppression_requirement, 4, 0.13).
narrative_ontology:measurement_basis(tech_su_t4, observed).
narrative_ontology:measurement(tech_su_t8, technology_diffusion_asymmetry, suppression_requirement, 8, 0.11).
narrative_ontology:measurement_basis(tech_su_t8, observed).
narrative_ontology:measurement(tech_su_t12, technology_diffusion_asymmetry, suppression_requirement, 12, 0.09).
narrative_ontology:measurement_basis(tech_su_t12, observed).
narrative_ontology:measurement(tech_su_t16, technology_diffusion_asymmetry, suppression_requirement, 16, 0.08).
narrative_ontology:measurement_basis(tech_su_t16, observed).
narrative_ontology:measurement(tech_su_t20, technology_diffusion_asymmetry, suppression_requirement, 20, 0.08).
narrative_ontology:measurement_basis(tech_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_diffusion_asymmetry, global_infrastructure).
narrative_ontology:boltzmann_floor_override(technology_diffusion_asymmetry, 0.08).
narrative_ontology:affects_constraint(technology_diffusion_asymmetry, export_control_effectiveness).
narrative_ontology:affects_constraint(technology_diffusion_asymmetry, asymmetric_warfare_capability_gap).
narrative_ontology:affects_constraint(technology_diffusion_asymmetry, dual_use_technology_governance).

% DUAL FORMULATION NOTE:
% This constraint is one component of a larger dual-use technology governance family. Related constraints include: export_control_effectiveness (the regulatory response to diffusion), asymmetric_warfare_capability_gap (the strategic outcome of diffusion), and dual_use_technology_governance (the institutional framework attempting to manage diffusion). Each has distinct ε values and should be modeled separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_diffusion_asymmetry, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
