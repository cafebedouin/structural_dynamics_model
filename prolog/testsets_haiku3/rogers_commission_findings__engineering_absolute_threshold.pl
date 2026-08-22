% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__engineering_absolute_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__engineering_absolute_threshold, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: rogers_commission_findings__engineering_absolute_threshold
 *   human_readable: Rogers Commission Engineering Safety Absolute: O-Ring Redesign Certification Requirement
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   The Rogers Commission findings after the Challenger disaster established
 *   a technical safety boundary: flight operations must cease until O-ring
 *   thermal redesign is certified as adequate. This reading interprets the
 *   constraint as an engineering absolute — a physically necessary condition
 *   for launch, not a probabilistic risk to be accepted by informed
 *   decision-makers (the actuarial_risk_acceptance reading) or a compliance
 *   process to be managed (the management_compliance_narrative reading). The
 *   engineering absolute reading treats the boundary as grounded in material
 *   failure physics: once the failure mode is understood, launching into that
 *   mode without remediation is structurally impermissible, regardless of
 *   risk quantification or management acceptance. The constraint suppresses
 *   launch operations at very high magnitude (0.92); extractiveness is low
 *   but non-zero because the constraint carries organizational cost (schedule
 *   delay, program overhead) that is borne by collective actors rather than
 *   individuals. Theater is minimal: the constraint is mostly functional —
 *   its operation consists of verifying redesign compliance, not performing
 *   symbolic safety review.
 *
 * KEY AGENTS:
 *   - Rogers Commission engineering authority: establishes and certifies the O-ring redesign boundary; authority derives from technical expertise in failure analysis
 *   - Flight crew: beneficiaries; safety protected by veto over launch under known hazard condition; trapped exit (cannot refuse launch without career cost)
 *   - NASA management: excluded from certification authority; politically constrained to accept the technical veto after public inquiry
 *   - Contractor engineering teams: bear redesign costs; gain veto authority in Flight Readiness Review going forward
 *   - Launch scheduling cadence: abstract payer; operations halted until redesign certified
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__engineering_absolute_threshold, 0.18).
domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, 0.92).
domain_priors:theater_ratio(rogers_commission_findings__engineering_absolute_threshold, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, extractiveness, 0.18).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__engineering_absolute_threshold, mountain).
narrative_ontology:human_readable(rogers_commission_findings__engineering_absolute_threshold, "Rogers Commission Engineering Safety Absolute: O-Ring Redesign Certification Requirement").
narrative_ontology:topic_domain(rogers_commission_findings__engineering_absolute_threshold, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__engineering_absolute_threshold).
domain_priors:emerges_naturally(rogers_commission_findings__engineering_absolute_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__engineering_absolute_threshold, '5264ac73-7279-4483-996f-8f881436e17f').
narrative_ontology:cs_kernel_codification('5264ac73-7279-4483-996f-8f881436e17f', formalized).
narrative_ontology:cs_authority_grounding('5264ac73-7279-4483-996f-8f881436e17f', expertise).
narrative_ontology:cs_interpretation_layer_present('5264ac73-7279-4483-996f-8f881436e17f').
narrative_ontology:cs_reading_relation('5264ac73-7279-4483-996f-8f881436e17f', rogers_commission_findings__actuarial_risk_acceptance, forecloses).
narrative_ontology:cs_reading_relation('5264ac73-7279-4483-996f-8f881436e17f', rogers_commission_findings__management_compliance_narrative, influences).
narrative_ontology:cs_axiom('5264ac73-7279-4483-996f-8f881436e17f', foundational, discovered_failure_mode_precludes_operation).
narrative_ontology:cs_axiom_status(discovered_failure_mode_precludes_operation, holdable).
narrative_ontology:cs_axiom_grounding('5264ac73-7279-4483-996f-8f881436e17f', discovered_failure_mode_precludes_operation, empirically_contingent).
narrative_ontology:cs_axiom('5264ac73-7279-4483-996f-8f881436e17f', foundational, engineering_certification_gates_launch_authority).
narrative_ontology:cs_axiom_status(engineering_certification_gates_launch_authority, holdable).
narrative_ontology:cs_axiom_grounding('5264ac73-7279-4483-996f-8f881436e17f', engineering_certification_gates_launch_authority, deontological).
narrative_ontology:cs_reference_frame('5264ac73-7279-4483-996f-8f881436e17f', physics_based_safety_boundary).
narrative_ontology:cs_drift_state('5264ac73-7279-4483-996f-8f881436e17f', contemporary_organizational_pressure_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('5264ac73-7279-4483-996f-8f881436e17f', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, flight_crew_safety_standard).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, flight_crew).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, contractor_engineering_teams).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, contractor_engineering_teams).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and certifies the O-ring thermal boundary and redesign specifications. Conducts post-failure analysis, sets the certification standard, and reviews all redesign proposals against the discovered failure mode. The authority derives from structural understanding of the failure physics, not from organizational hierarchy.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_engineering_authority, agenda_setter,
    institutional, generational, analytical, national).

% Safety boundary ensures they do not launch into a known failure mode. They cannot refuse launch without career consequences and have no independent verification authority. The constraint protects them by preventing launch approval under the discovered unsafe condition.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, flight_crew, beneficiary,
    powerless, immediate, trapped, national).

% Launch operations halt until redesign certification is complete. This is an abstract loss (postponement, schedule compression, program delays) borne by the organization's production objectives, not by individual actors but as a collective operational cost.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, launch_scheduling_cadence, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__engineering_absolute_threshold, launch_scheduling_cadence).

% Would prefer to manage risk probabilistically and maintain launch cadence; politically constrained to accept the engineering veto after the public inquiry. They are excluded from the certification authority itself and cannot override the technical boundary without explicit reversal of the Rogers findings — which carries political cost too high to contemplate.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, nasa_management, excluded,
    institutional, generational, constrained, national).

% Bear the cost of redesign and extended development cycles. Also benefit from the veto authority it grants engineering judgment in Flight Readiness Reviews going forward — the constraint shifts power from management-driven launch pressure toward engineer-led safety certification.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, contractor_engineering_teams, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__engineering_absolute_threshold, contractor_engineering_teams, beneficiary).

% Are not part of this constraint's immediate scope (Shuttle program specific) but face parallel organizational pressures. If the constraint erodes or is circumvented, they would experience increased pressure to launch despite unresolved safety questions; their exclusion from this boundary makes them witnesses to whether it holds.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, competing_launch_programs, excluded,
    institutional, generational, mobile, national).

% A transparent, binding technical constraint on launch operations restores confidence that safety decisions are not subordinated to schedule pressure. Public legitimacy of the space program depends on demonstrable commitment to the boundary, not on the boundary being circumvented for political gain.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, public_trust_in_spaceflight, beneficiary,
    moderate, generational, mobile, national).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__engineering_absolute_threshold, public_trust_in_spaceflight).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__engineering_absolute_threshold, diffuse).
narrative_ontology:fixing_cost_class(rogers_commission_findings__engineering_absolute_threshold, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, binding technical authority (Rogers engineering certification) to adjudicate whether a discovered failure mode has been remedied before launch can resume. Solves the coordination problem: who decides when the specific discovered hazard has been adequately addressed? The constraint answers: the engineering authority that discovered the hazard, not the schedule-keeper.
% TRANSFER_FUNCTION: Moves decision authority from management-driven Flight Readiness Review (schedule-influenced) to engineer-certified thermal boundary (physics-determined). The transfer is of veto power, not money: engineers gain authority to stop launch; management loses authority to override technical objection via risk acceptance framing.
% ABSENT_VOICES: Actuarial risk quantifiers and probabilistic safety analysts are excluded from the authority structure of this reading. They would argue for risk acceptance frameworks and probabilistic failure bounds; this reading forecloses their input into the launch certification decision by anchoring it to an absolute redesign requirement, not a risk metric. Competing launch programs are excluded structurally and would testify about whether organizational pressure erodes the boundary.
% DISAPPEARANCE_RATIONALE: If the engineering absolute boundary disappeared — if launches were permitted to resume without O-ring redesign certification — the Shuttle program would have resumed flights on the same hardware under a management risk-acceptance framework, and a second failure would have occurred (historical counterfactual: STS-51-L happened because the boundary was not in place; its prevention is the constraint's operational fact). The world reorganizes: either flights resume under the old thermal regime and risk materializes, or the boundary gets informally re-established through accumulated engineering objections that approximate the same veto authority.
% FOUNDING_PROBLEM: The Challenger failure revealed that organizational pressure for schedule had overridden engineering concerns about O-ring thermal behavior in cold conditions. The founding problem is: how can technical safety concerns be protected from being rationalized away by schedule pressure and cost-benefit risk acceptance?
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission (external investigative authority outside NASA management) documented the failure causation and established the technical boundary as non-negotiable. Subsequent Shuttle Program operations through 2011 operated under this boundary: no flights occurred until O-ring redesign and certification was complete. Independent aerospace safety analysis and engineering literature corroborate that the founding problem persists — organizational pressure for schedule creates standing tension against technical safety authority.
narrative_ontology:disappearance_verdict(rogers_commission_findings__engineering_absolute_threshold, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__engineering_absolute_threshold, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__engineering_absolute_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rogers_commission_findings__engineering_absolute_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__engineering_absolute_threshold, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, ExtMetricName, E),
    domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(rogers_commission_findings__engineering_absolute_threshold),
    narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claim is Mountain because the constraint is treated as emerging from material physics: once the O-ring thermal failure mode is discovered and characterized, launching without remediation becomes physically impermissible under the engineering reading. Suppression is extremely high (0.92) because the constraint operates as an absolute veto: launches cannot proceed under any risk-acceptance rationale or management decision-making framework so long as the discovered hazard is unresolved. The suppression is structural and non-negotiable — it is not attenuated by power or exit options because the boundary is anchored to a physical fact (the failure mode), not to organizational preference. Extractiveness is low (0.18) and rises gradually: the founding constraint carries only the cost of delayed schedule (reorganized as redesign cycles); as time passes and organizational pressure accumulates to resume operations, extractive elements begin to layer (corner-cutting on redesign verification, pressure to find an acceptable-risk interpretation of partial compliance). Theater is minimal (0.08): the constraint's operation is mostly functional — verifying redesign against the specified thermal boundary — rather than performative. The measurement series shows extractiveness rising slightly as years pass without return to flight (organizational pressure mounting, cost accumulation), while suppression and theater remain flat (the boundary itself does not erode, and symbolic compliance activity stays minimal — the constraint is robustly maintained because the failure physics is undisputed).
 *
 * PERSPECTIVAL GAP:
 *   From the engineering authority's seat, the constraint is straightforward: the failure mode is characterized, redesign is the necessary remedy, certification gates launch. No disagreement. From NASA management's seat, the constraint appears as an obstacle to program progress — a hard stop that persists regardless of risk quantification or management judgment. They experience it as externally imposed veto authority. From the flight crew's seat, it is unambiguous protection: they benefit and have no standing to object. From the contractor engineering teams' seat, it is mixed: burden of redesign work, but empowerment in future Flight Readiness Reviews. From the schedule/program cadence seat (abstract), it is pure cost with no upside. The engine computes these divergences from the declared roles and structural positions — the same constraint is experienced as natural boundary (engineering), external constraint (management), protection (crew), empowerment (engineers), and delay (schedule).
 *
 * DIRECTIONALITY LOGIC:
 *   Flight crew sits at full beneficiary (d ≈ 0.0): the constraint protects them without imposing cost on them; they have no say in whether it applies but gain unambiguous safety benefit. Launch cadence (abstract; schedule delay) sits at full target (d ≈ 1.0): the constraint imposes cost (postponement) with no offsetting benefit to the schedule itself — the cost is real and distributed across the program. NASA management sits at moderate-to-high target (d ≈ 0.65): they bear organizational cost (delayed program revenue, political pressure for progress) and are excluded from the certification authority; they cannot redefine the boundary without reversing the Rogers findings (high exit barrier). Contractor engineering teams sit at moderate-beneficiary (d ≈ 0.35): they bear redesign costs but gain veto authority in future Flight Readiness Reviews, shifting power toward engineering judgment and away from management-driven schedule pressure. The engine derives these d values from the beneficiary/victim declarations (flight crew is beneficiary, launch cadence/management/contractors bear costs) and the exit options (crew trapped, management constrained, contractors organized).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not resolve mandatrophy; instead, it *prevents* mandatrophy. The founding problem (organizational pressure overriding safety concerns) is structurally prevented by anchoring the launch decision to physics-based certification, not management risk-acceptance. Mandatrophy would occur if the constraint were maintained performatively after the founding problem no longer existed — i.e., if redesign were certified and the boundary were maintained theatrically to justify continued organizational overhead. The measurement series shows theater_ratio flat at ~0.08, indicating the constraint remains functional rather than becoming performative. If theater_ratio were to rise sharply (e.g., redesign certified but launches still delayed for ceremonial safety review), mandatrophy would be indicated. The constraint is robust against mandatrophy precisely because the beneficiary (flight crew safety) and the foundational requirement (O-ring redesign certification) are aligned: as long as the redesign is genuine, the constraint serves its founding function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engineering_absolute_vs_management_risk,
    'Is the O-ring redesign requirement a non-negotiable physics-based boundary, or is it a risk quantification framework that NASA management could override with informed acceptance?',
    'Historical: whether the constraint was maintained as binding when organizational pressure mounted, or whether exceptions were granted on a risk-acceptance basis. Counterfactual: if Rogers findings had recommended a probabilistic risk acceptance framework instead of an absolute redesign requirement, would NASA have operated under that framework?',
    'If negotiable by risk acceptance, the constraint reclassifies from Mountain to Tangled Rope or Snare (depending on who holds override authority). If truly absolute, it remains Mountain anchored to physics-based necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(engineering_absolute_vs_management_risk, empirical, 'Whether the constraint is physics-absolute or risk-negotiable').

omega_variable(
    natural_law_vs_constructed_authority,
    'Does the constraint emerge naturally from the material properties of O-rings under thermal stress, or is it constructed by the Rogers Commission''s institutional authority to interpret findings?',
    'Expertise audit: independent thermal and materials engineers confirm or dispute the claimed failure mode. Historical: whether the constraint persisted when Rogers Commission authority eroded (post-Challenger public attention, post-Columbia as pressures changed), or whether it remained because the physics is undisputed.',
    'If physics-grounded and expert consensus is stable, Mountain classification stands. If the constraint persists primarily through institutional authority (Rogers Commission legitimacy), reclassify to Rope (coordination around a shared authority) or Piton (maintained by institutional inertia after the authority eroded).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_authority, conceptual, 'Whether the constraint is grounded in material physics or in institutional authority').

omega_variable(
    engineering_veto_persistence,
    'Does the constraint durably empower engineering judgment in Flight Readiness Reviews, or is it a temporary post-disaster reaction that erodes as organizational pressure normalizes?',
    'Track engineering objection authority over multi-year horizon: do engineers retain veto power, or does management eventually override engineering objections using risk-acceptance rationales?',
    'Durable empowerment of engineering judgment indicates the constraint remains Mountain (a structural shift to physics-based decision-making). Erosion indicates the constraint is Piton (performative maintenance of post-disaster authority that management gradually reclaims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineering_veto_persistence, empirical, 'Whether the engineering veto authority is durable or eroded over time').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.92) structural — enforced by certification authority and organizational policy — or internalized in engineering judgment such that engineers would choose to oppose launch even without the formal boundary?',
    'Counterfactual: if the formal boundary were removed but engineering authority in Flight Readiness Review remained, would engineers still resist launching with unresolved O-ring concerns? If yes, the suppression is internalized in professional judgment; if no, it is purely structural.',
    'If internalized, the suppression is robust against institutional erosion — engineers carry the safety standard with them even if the formal constraint is circumvented. If structural, the suppression depends on maintenance of the formal authority and is vulnerable to institutional bypass.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, conceptual, 'Whether suppression is structurally enforced or internalized in professional judgment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__engineering_absolute_threshold, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0, 0.05).
narrative_ontology:measurement(roge_tr_t5, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 5, 0.06).
narrative_ontology:measurement(roge_tr_t10, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 10, 0.07).
narrative_ontology:measurement(roge_tr_t15, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 15, 0.08).
narrative_ontology:measurement(roge_tr_t20, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 20, 0.08).
narrative_ontology:measurement(roge_tr_t25, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 25, 0.08).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(roge_be_t5, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(roge_be_t10, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(roge_be_t15, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 15, 0.16).
narrative_ontology:measurement(roge_be_t20, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 20, 0.17).
narrative_ontology:measurement(roge_be_t25, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 25, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0, 0.91).
narrative_ontology:measurement(roge_su_t5, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 5, 0.92).
narrative_ontology:measurement(roge_su_t10, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 10, 0.92).
narrative_ontology:measurement(roge_su_t15, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 15, 0.92).
narrative_ontology:measurement(roge_su_t20, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 20, 0.92).
narrative_ontology:measurement(roge_su_t25, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 25, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__engineering_absolute_threshold, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rogers_commission_findings__engineering_absolute_threshold, 0.12).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__actuarial_risk_acceptance).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__management_compliance_narrative).

% DUAL FORMULATION NOTE:
% Rogers Commission kernel decomposed into three constraints: (1) engineering_absolute_threshold (THIS — physics-based, Mountain-class) establishes O-ring redesign as non-negotiable; (2) actuarial_risk_acceptance (Tangled Rope-class) reinterprets findings as risk-quantification requirement; (3) management_compliance_narrative (Rope-class) treats findings as process documentation requirement. All three readings share the same kernel (Rogers investigation) but yield different ε values, beneficiary/victim structures, and classifications. This reading forecloses the actuarial reading within any single-authority framework and influences the management reading's authority structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
