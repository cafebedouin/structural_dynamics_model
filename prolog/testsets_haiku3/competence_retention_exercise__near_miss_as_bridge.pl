% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__near_miss_as_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__near_miss_as_bridge, []).

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
 *   constraint_id: competence_retention_exercise__near_miss_as_bridge
 *   human_readable: Hybrid Competence Retention: Simulation + Near-Miss Integration
 *   domain: safety/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested
 *   'competence_retention_exercise' kernel: near-misses and minor failures
 *   supply sufficient real-world feedback to validate and update simulator
 *   training without requiring catastrophic events. The reading claims that a
 *   hybrid system — combining routine simulator drilling with active
 *   near-miss investigation and integration — maintains catastrophe-avoidance
 *   competence sustainably. This reading is distinct from siblings that frame
 *   competence maintenance either as simulation-sufficient or as
 *   catastrophe-necessary. The constraint itself is a ROPE: genuine
 *   coordination function (solving the skill-maintenance problem),
 *   distributed beneficiaries (organization + workforce), and active
 *   enforcement (near-miss reporting and simulator updates must be
 *   maintained). The core extraction is modest (0.38) and arises from
 *   identity-lock costs borne by near-miss reporters and administrative
 *   burden on simulator maintainers, not from asymmetric rent collection.
 *
 * KEY AGENTS:
 *   - safety_organization: institutional agenda-setter; designs and maintains the hybrid protocol; collects the organizational learning benefit; carries administrative overhead
 *   - competent_workforce: organized beneficiary; gains real-world calibration from near-miss debriefs and procedural fluency from simulator drilling; identity-locked into the transparency requirement
 *   - near_miss_reporters: moderate-power payers; front-line personnel bearing emotional and professional costs of incident admission and investigation; career/identity risk if involved in near-miss
 *   - simulator_maintainers: powerful payers; manage the technical burden of near-miss integration and fidelity iteration; have mobile exit options
 *   - catastrophe_traditionalists: excluded institutional voice advocating disaster-based learning; retain veto power over funding
 *   - pure_simulation_advocates: excluded institutional voice arguing near-miss investigation is wasteful overhead; would compete for resources if included
 *   - regulatory_bodies: observer seats; audit competence outcomes and can mandate protocol changes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, 0.38).
domain_priors:suppression_score(competence_retention_exercise__near_miss_as_bridge, 0.22).
domain_priors:theater_ratio(competence_retention_exercise__near_miss_as_bridge, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, extractiveness, 0.38).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__near_miss_as_bridge, rope).
narrative_ontology:human_readable(competence_retention_exercise__near_miss_as_bridge, "Hybrid Competence Retention: Simulation + Near-Miss Integration").
narrative_ontology:topic_domain(competence_retention_exercise__near_miss_as_bridge, "safety/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__near_miss_as_bridge).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__near_miss_as_bridge, '88e8ea47-a9ed-4ac3-9b52-d66422f94690').
narrative_ontology:cs_kernel_codification('88e8ea47-a9ed-4ac3-9b52-d66422f94690', distributed).
narrative_ontology:cs_authority_grounding('88e8ea47-a9ed-4ac3-9b52-d66422f94690', expertise).
narrative_ontology:cs_interpretation_layer_present('88e8ea47-a9ed-4ac3-9b52-d66422f94690').
narrative_ontology:cs_reading_relation('88e8ea47-a9ed-4ac3-9b52-d66422f94690', competence_retention_exercise__simulation_as_sufficient, influences).
narrative_ontology:cs_reading_relation('88e8ea47-a9ed-4ac3-9b52-d66422f94690', competence_retention_exercise__catastrophe_as_necessary, coexists_with).
narrative_ontology:cs_axiom('88e8ea47-a9ed-4ac3-9b52-d66422f94690', foundational, near_miss_provides_sufficient_calibration).
narrative_ontology:cs_axiom_status(near_miss_provides_sufficient_calibration, holdable).
narrative_ontology:cs_axiom_grounding('88e8ea47-a9ed-4ac3-9b52-d66422f94690', near_miss_provides_sufficient_calibration, empirically_contingent).
narrative_ontology:cs_axiom('88e8ea47-a9ed-4ac3-9b52-d66422f94690', secondary, hybrid_system_superior_to_pure_alternatives).
narrative_ontology:cs_axiom_status(hybrid_system_superior_to_pure_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('88e8ea47-a9ed-4ac3-9b52-d66422f94690', hybrid_system_superior_to_pure_alternatives, instrumental).
narrative_ontology:cs_reference_frame('88e8ea47-a9ed-4ac3-9b52-d66422f94690', competence_maintenance_via_real_world_feedback).
narrative_ontology:cs_drift_state('88e8ea47-a9ed-4ac3-9b52-d66422f94690', contemporary_organizational_learning_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('88e8ea47-a9ed-4ac3-9b52-d66422f94690', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, safety_organization).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, competent_workforce).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, near_miss_reporters).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, simulator_maintainers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and maintains the hybrid training protocol: designs simulators, establishes near-miss reporting channels, analyzes incident data, and integrates findings back into curriculum. Justifies the protocol as evidence-based competence maintenance. Carries the administrative overhead of near-miss investigation and simulator iteration but collects the organizational benefit of sustained competence and reduced catastrophic risk.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_organization, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, safety_organization, beneficiary).

% Participates in both simulator exercises and near-miss investigation debriefs. Maintains procedural competence through repeated simulator drilling. Gains real-world calibration by learning from near-miss cases: understanding what the early signs looked like, how the organization responded, what recovery looked like. Their competence is the constraint's primary product.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, competent_workforce, beneficiary,
    organized, biographical, constrained, global).

% Front-line personnel (pilots, surgeons, operators, engineers) who encounter near-misses and are required to report them for investigation and integration into training. They bear the emotional and professional cost of admitting and explaining the incident, the time cost of debriefs, and the identity cost of being known as someone who 'almost failed.' Their professional identity is entangled with the learning system's competence claims.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, near_miss_reporters, payer,
    moderate, biographical, identity_locked, global).

% Technical staff who maintain simulator fidelity and update scenarios based on near-miss findings. Their work is driven by the need for continuous incorporation of real-world data; as near-miss volume rises, their maintenance burden escalates. They have expertise-driven exit options and can move between organizations if the burden becomes unmanageable.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, simulator_maintainers, payer,
    powerful, biographical, mobile, global).

% Organizational voices (sometimes entrenched in senior leadership or regulatory bodies) who believe only actual catastrophic events provide genuine learning stakes; they view near-miss-driven training as insufficient and advocate for disaster-based curriculum refresh. They are excluded from the protocol design but retain veto power over funding and policy.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, catastrophe_traditionalists, excluded,
    institutional, generational, trapped, global).

% Advocates for full-fidelity simulation as sufficient; they argue near-miss investigation and integration are unnecessary overhead and that high-fidelity simulators alone provide genuine competence maintenance. They are excluded from the decision to adopt the hybrid model but would compete for resources if given voice.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, pure_simulation_advocates, excluded,
    institutional, generational, mobile, global).

% Audit and certify competence-maintenance protocols. They observe whether the hybrid model achieves stated learning outcomes and whether catastrophic failure rates decline. They can mandate protocol changes or impose reporting requirements that alter the constraint's operation.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, regulatory_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__near_miss_as_bridge, safety_organization).
narrative_ontology:fixing_cost_class(competence_retention_exercise__near_miss_as_bridge, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the organizational learning problem: how to maintain catastrophe-avoidance competence in systems where real catastrophes are rare, fidelity-testing is dangerous, and muscle-memory atrophy is a silent threat. The hybrid protocol coordinates three knowledge streams — simulator drilling, near-miss data, and rare real incidents — into a single competence signal that the organization can measure and the workforce can trust.
% TRANSFER_FUNCTION: Moves organizational attention and front-line effort from catastrophe-response (reactive, traumatic, rare) toward near-miss investigation and simulator-based skill maintenance (proactive, distributed, continuous). Front-line personnel invest emotional and temporal labor in near-miss debriefs; the organization invests administrative and technical capacity in scenario updates. The transfer is from reactive crisis management to sustained competence discipline.
% ABSENT_VOICES: Catastrophe-experienced personnel (survivors of prior organizational failures) are partially excluded — their testimony about the inadequacy of non-catastrophic training is sometimes dismissed as survivorship bias or emotional residue rather than valid data. Pure-simulation advocates with high-fidelity sim engineering expertise are excluded from the core decision logic, though they resource it. Workforce subgroups with aversion to transparency (those who fear being identified in near-miss reports) are not present in the design conversation.
% DISAPPEARANCE_RATIONALE: If the hybrid protocol disappeared overnight — no near-miss integration, simulator updates frozen at current level, reporting channels closed — competence would begin atrophying within months (procedural memory decay is documented at 6–12 month horizons for high-consequence skills); near-miss patterns that would have been caught would escalate to minor incidents and then major ones; the organization would face pressure to either re-institute near-miss investigation or accept elevated catastrophic risk. The constraint's absence would force explicit choice between competence maintenance cost and catastrophic risk tolerance.
% FOUNDING_PROBLEM: High-reliability systems (aviation, surgery, nuclear operations) face a paradox: the skills required to prevent catastrophes can only be maintained through repeated practice in realistic conditions, but catastrophes themselves are rare and dangerous. Simulators partially solve this but cannot capture all environmental complexity and psychological authenticity. Near-misses are abundant signals of system-state and human-factor stressors; they offer real-world grounding without catastrophic consequence. The founding problem is: how to keep competence sharp when the real thing is too rare and too dangerous to use as training.
% FOUNDING_PROBLEM_CORROBORATION: Aviation safety data (NASA ASRS, accident investigation boards), surgical morbidity-and-mortality conferences, and nuclear operations experience reports all document that near-miss investigation and simulator integration are associated with sustained competence and reduced serious-incident rates. Personnel from these domains (not beneficiaries of the protocol's adoption, but experiencers of it) attest the problem is live and the hybrid model addresses it. This is corroborated outside the safety-organization beneficiary set by independent safety researchers and regulatory auditors.
narrative_ontology:disappearance_verdict(competence_retention_exercise__near_miss_as_bridge, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__near_miss_as_bridge, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__near_miss_as_bridge, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__near_miss_as_bridge, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__near_miss_as_bridge, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__near_miss_as_bridge_tests).
:- end_tests(competence_retention_exercise__near_miss_as_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.28) because the constraint delivers genuine coordination benefit: the organization solves a real organizational learning problem, and the workforce gains competence maintenance without catastrophic risk. However, extractiveness rises modestly (0.38 at interval end) as the constraint matures. The rise reflects: (1) administrative burden on simulator maintainers escalates as near-miss volume and integration complexity grow; (2) front-line personnel accumulate identity costs from repeated near-miss involvement; (3) the organization begins using near-miss data for performance evaluation, not just learning, introducing surveillance pressure. Suppression remains low (0.22) because the constraint operates via transparency and voluntary reporting, not coercion — though identity-lock on reporters constitutes structural suppression (the threat of being identified as someone who 'almost failed' deters some reporting). Theater ratio is low (0.18) because both simulator drilling and near-miss investigation perform real functions; the modest theater component is performative safety communication (organizational messaging about competence commitment) layered over genuine learning activity. The measurements track the constraint's maturation: as the system becomes established, administrative burden rises and identity-lock pressures intensify, but the core coordination function remains stable. All metrics are authored on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The safety-organization and regulator see 'rope' (coordination solves a real problem, enforcement is reasonable); near-miss reporters see 'tangled rope' (coordination benefit is real but personal costs are high and non-voluntary); catastrophe traditionalists see 'snare' (false security, inadequate reality-testing); pure-simulation advocates see 'inefficient rope' (the near-miss overhead extracts without improving learning). These are not disagreements about metrics — all seats observe the same extractiveness, suppression, and theater metrics — but disagreements about whether the constraint's structure is genuinely coordinative or extractive-under-coordination-cover. The engine distinguishes these seats by their structural position; commentary explains why the same constraint produces divergent type classifications across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The safety_organization is a structural beneficiary (d ≈ 0.2): it sets the protocol, incurs moderate overhead, and collects the core organizational benefit (maintained competence, reduced catastrophic risk). The competent_workforce is symmetric to near-beneficiary (d ≈ 0.35): they gain genuine competence and professional development, but bear modest identity-lock costs and mandatory reporting participation. Near_miss_reporters carry high directionality (d ≈ 0.75): they supply real-world data essential to the coordination function but bear the highest identity and emotional costs; their exit is identity-locked (admitting to a near-miss is entangled with their professional self-concept, and leaving the workforce or organization is the only full exit). Simulator_maintainers are moderate payers (d ≈ 0.55): they contribute technical labor and bear escalating burden, but have mobile exit options and professional-development benefits from the technical work.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (maintaining catastrophe-avoidance competence when real catastrophes are rare and dangerous) is live and remains so across the 30-year interval. The protocol persists because it solves this problem in an organizationally affordable way: it avoids both the cost of waiting for catastrophes and the hollowness of pure simulation. However, there is an incipient mandatrophy signal: as the protocol matures and near-miss data accumulates, the organization begins using near-miss data for performance evaluation and career-impact assessment, not just learning. This introduces secondary extraction (using learning data as surveillance) that is not intrinsic to the founding problem. The constraint shows no signs of becoming a piton (it is still actively maintained and performs real function), but the theater ratio's rise and the identity-lock's hardening suggest the extraction component is beginning to exceed the coordination cost. Mandatrophy is not yet declared, but the measurement trajectory and emerging surveillance use are amber signals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fidelity_sufficiency_boundary,
    'Do near-miss incidents provide psychologically and organizationally sufficient ''reality'' to maintain catastrophe-avoidance competence, or is the existential shock and full emotional stakes of catastrophe structurally necessary for genuine learned competence?',
    'Longitudinal cohort study comparing competence retention and incident rates in organizations using hybrid (near-miss + simulation) protocols vs. those using catastrophe-based curriculum refresh. Multi-decade follow-up to detect atrophied competence in rare-incident domains.',
    'If near-misses suffice, the hybrid model is validated and scaled. If catastrophe-level events are necessary, the constraint is reclassified as simulation-plus-near-miss theater with authentic learning only from true catastrophes — shifting from rope toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fidelity_sufficiency_boundary, empirical, 'Whether near-miss feedback is psychologically and organizationally sufficient for competence maintenance.').

omega_variable(
    reporting_suppression_mechanism,
    'Is the identity-lock on near-miss reporters structural (an unavoidable feature of real-world incident involvement) or internalized (a learned fear of organizational punishment that persists even in psychologically safe reporting environments)?',
    'Experimental manipulation of organizational transparency and non-punitive culture around near-miss reporting; measure whether reporting rates and depth change when identity protections are strengthened. Post-exit surveys of reporters to assess whether identity-lock persists after leaving the organization.',
    'If identity-lock is purely structural (inherent to being known as someone who had an incident), the suppression metric is accurate as authored. If partially internalized, the true suppression is lower in psychologically safe contexts, and the constraint''s nature is more ''rope'' than ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reporting_suppression_mechanism, empirical, 'Whether suppression is structural or internalized in near-miss reporting.').

omega_variable(
    simulation_adequacy_as_reading,
    'Is this reading''s claim that near-misses are necessary to validate simulators foreclosed by the pure-simulation reading''s claim that fidelity-equivalent simulation constitutes complete competence testing?',
    'Clarification of what each reading means by ''sufficient'': does ''sufficient'' mean ''maintains competence to regulatory standard'', ''achieves subjective confidence equivalent to real-world experience'', or ''prevents catastrophic failure''? If the readings disagree on the goal, they coexist; if they agree on the goal but disagree on the path, one forecloses the other.',
    'If readings foreclose each other, the kernel has a logical structure that favors one reading over others. If they coexist, the constraint is contested but structurally stable within the organizational choice space.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(simulation_adequacy_as_reading, conceptual, 'Structural relationship between this reading and the simulation-sufficient reading: logical foreclosure or coexistence.').

omega_variable(
    surveillance_creep_in_near_miss_integration,
    'As near-miss data accumulates and organizational use shifts from pure learning to performance evaluation, does the constraint transition from rope to snare — using the learning system as a surveillance apparatus?',
    'Track organizational policy over time: is near-miss involvement used in personnel evaluation, promotion, or disciplinary decisions? Interview near-miss reporters about whether they perceive their data as used for learning or performance assessment. Measure reporting rates as surveillance use increases.',
    'If surveillance creep occurs and goes unaddressed, the constraint''s extraction component rises and it reclassifies toward snare. If the organization maintains learning-only use boundaries, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surveillance_creep_in_near_miss_integration, empirical, 'Whether near-miss data accumulation leads to surveillance function that undermines the learning mandate.').

omega_variable(
    kernel_reading_foreclosure_catastrophe,
    'Does the ''catastrophe_as_necessary'' reading''s core claim — that only existential shock of catastrophe produces genuine organizational learning — foreclose this reading''s hybrid-model alternative?',
    'Logical analysis: can both readings coexist in a single decision-making framework? If an organization adopts the near-miss-hybrid model but preserves psychological space for the catastrophe reading (acknowledging that catastrophes may teach things hybrids cannot), the readings coexist. If the organization fully commits to near-miss-sufficiency and rejects catastrophe learning as unnecessarily risky, this reading forecloses the other.',
    'If readings foreclose, one is superior within the safety-organization''s institutional commitment; if they coexist, the organization remains contested between two live alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_catastrophe, conceptual, 'Foreclosure relationship between near-miss-as-bridge and catastrophe-as-necessary readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__near_miss_as_bridge, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0, 0.08).
narrative_ontology:measurement(comp_tr_t5, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 5, 0.11).
narrative_ontology:measurement(comp_tr_t10, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 10, 0.14).
narrative_ontology:measurement(comp_tr_t15, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 15, 0.16).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 20, 0.17).
narrative_ontology:measurement(comp_tr_t30, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 30, 0.18).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(comp_be_t5, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(comp_be_t10, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(comp_be_t15, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 15, 0.37).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(comp_be_t30, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(comp_su_t5, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 5, 0.17).
narrative_ontology:measurement(comp_su_t10, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 10, 0.19).
narrative_ontology:measurement(comp_su_t15, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 20, 0.21).
narrative_ontology:measurement(comp_su_t30, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 30, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__near_miss_as_bridge, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__near_miss_as_bridge, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__catastrophe_as_necessary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'competence_retention_exercise' kernel, which decomposes into three structurally distinct claims about how to maintain catastrophe-avoidance competence: (1) simulation_as_sufficient — fidelity-equivalent simulation is complete; (2) catastrophe_as_necessary — only catastrophes teach genuinely; (3) near_miss_as_bridge — near-misses plus simulation is the optimal hybrid. The three readings have different ε values (simulation has near-zero extraction, catastrophe has high extraction due to its human cost, near-miss has moderate extraction from identity-lock and administrative burden). The three readings also have different victim/beneficiary structures and different epistemic bases (fidelity-testing vs. existential-shock vs. real-world-calibration). Each reading is a separate constraint story with its own `cs_structure` fields documenting the kernel context, reading relations, and axioms. This story documents the near-miss-as-bridge reading; the sibling stories document the other two readings. All three are linked via `network.affects_constraints`.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_retention_exercise__near_miss_as_bridge, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
