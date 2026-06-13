% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__hybrid_near_miss_learning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__hybrid_near_miss_learning, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_avoidance_retention__hybrid_near_miss_learning
 *   human_readable: Hybrid Near-Miss Learning Constraint for Catastrophe Competence
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   High-reliability domains (aviation, nuclear energy) maintain practitioner
 *   competence through distributed learning networks that aggregate
 *   near-misses, foreign incidents, and high-fidelity drill scenarios. This
 *   reading of the catastrophe-avoidance kernel asserts that neither
 *   simulation alone (insufficient realism and selection pressure) nor
 *   catastrophe alone (too infrequent and costly) suffices for competence
 *   retention; the constraint is the orchestrated combination of near-miss
 *   aggregation, cross-organizational incident access, and regulatory
 *   enforcement of corrective action. This reading is in direct tension with
 *   siblings that privilege simulation as a proxy catastrophe or that argue
 *   only real catastrophes provide adequate selection pressure. The kernel
 *   contest reflects a fundamental disagreement about what constitutes
 *   sufficient evidence of competence and sufficient pressure to maintain it.
 *
 * KEY AGENTS:
 *   - Safety learning organizations (FAA, NTSB, ICAO, medical quality boards) — set the agenda for what counts as a learning-worthy incident and enforce participation
 *   - Incident reporting networks (ASRS, aviation safety databases) — aggregate and broadcast near-miss and foreign incident data across organizational boundaries
 *   - Practitioners with incident access (pilots, controllers, surgeons in strong-sharing domains) — benefit from distributed learning; pay the cost of disclosure and external scrutiny
 *   - Siloed practitioners (medical, manufacturing domains without robust sharing) — trapped outside the learning network; depend on local rediscovery or catastrophic selection
 *   - Regulators and investigative authorities — select which incidents get investigated and which findings cascade as mandatory corrective action; dual agenda-setter/beneficiary role
 *   - Simulation and drill operators — provide the high-realism bridge between incident learning and catastrophic selection; they see the constraint as incomplete
 *   - Catastrophe survivors and bereaved (excluded) — would testify that learning velocity is insufficient; locked out of network governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.58).
domain_priors:suppression_score(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.42).
domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__hybrid_near_miss_learning, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__hybrid_near_miss_learning, "Hybrid Near-Miss Learning Constraint for Catastrophe Competence").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__hybrid_near_miss_learning, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__hybrid_near_miss_learning).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__hybrid_near_miss_learning, '21f334c3-3f49-42fd-8dd8-5f6a39dc4e11').
narrative_ontology:cs_kernel_codification('21f334c3-3f49-42fd-8dd8-5f6a39dc4e11', distributed).
narrative_ontology:cs_authority_grounding('21f334c3-3f49-42fd-8dd8-5f6a39dc4e11', extraction).
narrative_ontology:cs_interpretation_layer_present('21f334c3-3f49-42fd-8dd8-5f6a39dc4e11').
narrative_ontology:cs_reading_relation('21f334c3-3f49-42fd-8dd8-5f6a39dc4e11', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, coexists_with).
narrative_ontology:cs_reading_relation('21f334c3-3f49-42fd-8dd8-5f6a39dc4e11', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, influences).
narrative_ontology:cs_axiom('21f334c3-3f49-42fd-8dd8-5f6a39dc4e11', foundational, distributed_incident_learning_suffices_for_competence).
narrative_ontology:cs_axiom_status(distributed_incident_learning_suffices_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('21f334c3-3f49-42fd-8dd8-5f6a39dc4e11', distributed_incident_learning_suffices_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('21f334c3-3f49-42fd-8dd8-5f6a39dc4e11', foundational, regulatory_incident_aggregation_necessary_coordination).
narrative_ontology:cs_axiom_status(regulatory_incident_aggregation_necessary_coordination, holdable).
narrative_ontology:cs_axiom_grounding('21f334c3-3f49-42fd-8dd8-5f6a39dc4e11', regulatory_incident_aggregation_necessary_coordination, instrumental).
narrative_ontology:cs_axiom('21f334c3-3f49-42fd-8dd8-5f6a39dc4e11', secondary, catastrophe_avoidable_through_near_miss_learning).
narrative_ontology:cs_axiom_status(catastrophe_avoidable_through_near_miss_learning, holdable).
narrative_ontology:cs_axiom_grounding('21f334c3-3f49-42fd-8dd8-5f6a39dc4e11', catastrophe_avoidable_through_near_miss_learning, empirically_contingent).
narrative_ontology:cs_reference_frame('21f334c3-3f49-42fd-8dd8-5f6a39dc4e11', distributed_incident_learning_as_sufficient).
narrative_ontology:cs_drift_state('21f334c3-3f49-42fd-8dd8-5f6a39dc4e11', contemporary_regulatory_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('21f334c3-3f49-42fd-8dd8-5f6a39dc4e11', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_learning_organizations).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, incident_reporting_networks).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, organizations_without_incident_access).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, siloed_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, practitioners_with_incident_access).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, regulators_and_investigative_authorities).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, practitioners_with_incident_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the incident investigation and aggregation infrastructure (FAA, NTSB, ICAO, airline safety boards). They decide what gets investigated with depth, which incidents are worth reporting, and which corrective actions cascade as mandatory requirements. They set the selection pressure for competence maintenance.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_learning_organizations, agenda_setter,
    organized, generational, mobile, global).

% ASRS, aviation safety reporting systems, accident investigation databases. They aggregate raw incident data, standardize taxonomies, and broadcast learning across organizational boundaries. They benefit by operating as the authoritative intermediary.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, incident_reporting_networks, beneficiary,
    institutional, generational, mobile, global).

% Pilots, air traffic controllers, nuclear operators, surgeons in domains with strong incident-sharing (aviation, nuclear). They gain access to near-miss learning from peer organizations and foreign sources, enabling competence maintenance without local catastrophes. They also bear reporting requirements, liability exposure from disclosure, and loss of operational autonomy.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, practitioners_with_incident_access, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, practitioners_with_incident_access, payer).

% Healthcare providers, industrial workers, cybersecurity specialists in domains without robust cross-organizational incident-sharing. They cannot access the learning infrastructure available to aviation; they rediscover hazards locally or depend on surviving catastrophes. Their exit is locked by professional identity and by the absence of alternative learning networks.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, siloed_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Government agencies (FAA, NTSB, medical boards) that determine investigation depth, mandatory corrective action, and enforcement velocity. They set the agenda for what counts as a learning-worthy incident and extract legitimacy from managing safety outcomes.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, regulators_and_investigative_authorities, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, regulators_and_investigative_authorities, beneficiary).

% Organizations in domains with weak incident-sharing infrastructure or regulatory gaps. They pay the cost of not learning: higher local incident rates, slower corrective cycles, and degrading competence over time. Exit is constrained by regulatory requirement to operate in their domain.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, organizations_without_incident_access, payer,
    moderate, biographical, trapped, national).

% Training organizations, simulator manufacturers, high-fidelity drill coordinators. They see the constraint as incomplete: simulation alone lacks realism, incident-sharing alone lacks selection pressure. They provide the bridge between learning and competence maintenance.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_and_drill_operators, observer,
    organized, generational, analytical, global).

% Individuals and families affected by preventable catastrophes that the learning network did not stop. They would testify that the constraint's learning velocity is insufficient and that competence gaps persist. They are structurally locked out of the agenda-setting layer that determines what gets learned and how fast.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_survivors_and_bereaved, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_learning_organizations).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__hybrid_near_miss_learning, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates near-miss and foreign incident data across organizational boundaries, preventing each organization from re-discovering the same hazards independently and enabling practitioners to learn without waiting for local catastrophes.
% TRANSFER_FUNCTION: Transfers operational autonomy and proprietary incident knowledge from individual organizations to regulatory authorities; transfers competence-maintenance risk from organizations with incident access to those without it; transfers investigative burden from organizations to authorities.
% ABSENT_VOICES: Organizations in domains without incident-sharing infrastructure (much of healthcare, manufacturing, cybersecurity) and people harmed by preventable catastrophes that the network failed to stop. They would argue for mandatory participation, faster corrective action, and genuine transparency about why learning velocity is insufficient.
% DISAPPEARANCE_RATIONALE: If incident-sharing networks disappeared, practitioners would lose access to peer and foreign incident data; competence maintenance would revert to local rediscovery and catastrophic selection. Incident rates would rise sharply in domains that rely on the network. Practitioners would require more intensive simulation or higher tolerance for catastrophic losses.
% FOUNDING_PROBLEM: High-reliability domains discovered that competence maintenance required learning from near-misses and incidents beyond any single organization's experience; catastrophic selection events were too frequent and too costly to be the sole competence maintenance mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Aviation safety authorities (FAA, ICAO, NTSB) and independent safety researchers attest the problem is live: incident-sharing prevents predictable catastrophes. Comparative studies of aviation versus healthcare show the correlation between incident-sharing strength and accident-rate reduction. Simulation operators and some practitioners attest the constraint is incomplete without additional mechanisms (catastrophic selection or high-realism drills), introducing the contested dimension about sufficiency.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__hybrid_near_miss_learning, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__hybrid_near_miss_learning, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and rising (0.35 → 0.58 over interval) because the constraint consolidates control over competence maintenance into the hands of regulatory authorities and incident-sharing networks; organizations and practitioners without incident access lose autonomy over their own learning. The rise reflects gradual expansion of what gets reported, investigated, and mandated as corrective action — the constraint's reach deepens. Suppression is lower than extractiveness (0.42) because participants generally view incident-sharing as legitimate and competence-enhancing; however, active suppression of certain incident types (organizational embarrassment, liability exposure) and of alternative learning pathways (proprietary incident databases, internal-only review) is necessary to maintain the network's coherence. Theater ratio is moderate (0.31) because a real coordination function (distributed learning) coexists with performative compliance (incidents reported but not acted on, corrective actions mandated but not implemented). The measurement series show steady increases, modeling the constraint's expansion and deepening enforcement over a 50-year interval consistent with the historical growth of aviation safety infrastructure post-1945.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (safety learning organizations, regulators) and the practitioner seats experience this constraint very differently. From the agenda-setter view, the constraint is essential coordination that prevents organizational insularity and catastrophic rediscovery. From the practitioner view, it is a asymmetric power grab: safety authorities control what counts as knowledge and what gets mandated, practitioners bear the cost of disclosure, and the constraint persists because it benefits authorities more than it benefits practitioners (who would prefer local autonomy). Organizations without incident access (siloed practitioners) experience it as pure extraction — they pay the cost of exclusion (higher risk, slower learning) without receiving the benefit. The engine computes directionality per-seat: beneficiaries (practitioners with access + authorities) get d near the beneficiary end; payers (siloed practitioners, organizations without access) get d near the target end. This divergence is the core of the tangled-rope structure: the same constraint coordinates learning for insiders and extracts competence-maintenance risk from outsiders.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: practitioners_with_incident_access and safety_learning_organizations sit at d ≈ 0.15-0.35 (they benefit from the coordination; their exit is constrained but meaningful — they could exit the domain or the network). Regulators and authorities sit at d ≈ 0.20-0.40 (they benefit from control, constrained exit because they are institutionally committed to the domain). Victims: siloed_practitioners sit at d ≈ 0.75-0.85 (locked in by identity and regulatory requirement; extraction risk from being outside the learning network is structural). organizations_without_incident_access sit at d ≈ 0.70-0.80 (trapped by regulatory prohibition; they must operate in their domain but cannot access the learning infrastructure). The override for siloed_practitioners is identity_locked exit: they cannot leave medicine or their specialty without abandoning professional identity; the constraint's extraction pressure is sustained by that lock. This is not a directionality_overrides entry but rather a core feature of the story: identity lock is the mechanism that keeps them extractively positioned despite whatever other factors might lower d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (learning from near-misses without waiting for catastrophe) is live in aviation and remains the constraint's justification. However, the constraint has acquired secondary functions that complicate the mandate. Regulatory capture phenomena: authorities expand what counts as reportable to increase investigative scope (filling the space with work); organizations expand simulation and drill infrastructure to appear compliant while resisting deeper incident-sharing. The constraint could be reconfigured to increase learning velocity (mandatory real-time incident broadcast, expedited root-cause analysis, faster corrective cascading) but the current form has settled into a steady-state rhythm that benefits the authorities' administrative infrastructure more than practitioners' actual competence. The theater ratio rising from 0.15 to 0.31 models this: the operational core of learning (near-miss aggregation, corrective action) remains real, but an increasing share of the constraint's energy goes into performative reporting, compliance documentation, and maintaining the authorities' legitimacy. This is not mandatrophy-resolved (the founding problem is still live and still driving the structure), but it shows the early stages of what could become mandatrophy: the authorities running the network are now the primary beneficiaries, and practitioners are paying costs that exceed the learning benefits they receive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_as_proxy_catastrophe,
    'Is high-fidelity simulation a functionally equivalent proxy for catastrophic selection, or does it systematically fail to generate the psychological, organizational, and systemic learning that real catastrophes produce?',
    'Comparative competence outcome studies: measure practitioner and organizational competence (error rates, incident rates, recovery time) in domains that rely heavily on simulation versus those that have experienced recent real catastrophes. Track whether simulated catastrophes generate the same corrective urgency and cultural change as real ones.',
    'If simulation IS equivalent, near-miss learning networks could be supplemented or partially replaced by cheaper, more accessible simulation infrastructure, reducing the constraint''s extraction intensity and unlocking domains without incident-sharing networks. If simulation is NOT equivalent, the constraint''s necessity becomes stronger and its reach justifiably expands; the constraint may even fail to maintain competence in domains that rely on simulation alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_as_proxy_catastrophe, empirical, 'Whether simulation constitutes genuine practice or a fundamentally different learning modality.').

omega_variable(
    catastrophe_necessity_ambiguity,
    'Is actual catastrophic selection pressure necessary for competence maintenance, or is it merely historically how competence was maintained before incident-sharing networks existed?',
    'Long-term comparative analysis: aviation (with strong incident-sharing networks) versus hypothetical counterfactual domain where near-miss access is restricted but catastrophes continue. Track whether competence degradation occurs in absence of incident-sharing, controlling for catastrophic selection.',
    'If catastrophe IS necessary (even with near-miss access), the constraint is incomplete; practitioners in domains without catastrophe experience (nascent nuclear energy, novel aerospace designs) will suffer competence degradation regardless of incident-sharing. If catastrophe is NOT necessary, the constraint''s power to maintain competence without catastrophic selection is vindicated, and the sibling reading (catastrophe_as_necessary_selector) is largely foreclosed. This directly challenges the kernel contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophe_necessity_ambiguity, empirical, 'Whether catastrophic selection is inherently necessary or was historically contingent.').

omega_variable(
    learning_velocity_sufficiency,
    'Is the current velocity of near-miss learning sufficient to prevent competence degradation over the time horizons practitioners operate in (decades for individuals, centuries for organizations)?',
    'Measure incident-to-corrective-action cycle times and compare against the decay timescale of competence in absence of reinforcement. Track whether organizations that miss the incident-sharing network (by geographic isolation, regulatory gaps, or proprietary barriers) show competence degradation relative to those with full access.',
    'If velocity is insufficient, the constraint fails its core function; practitioners will still require catastrophic selection or more intensive simulation. If velocity is sufficient for some domains but not others (aviation yes, healthcare no), it suggests the constraint is necessary but not evenly distributed — some seats are buying competence maintenance cheaply while others are denied access.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(learning_velocity_sufficiency, empirical, 'Whether the near-miss learning rate sustains competence maintenance.').

omega_variable(
    incident_access_exclusion_mechanism,
    'Why are incident-sharing networks strong in aviation and weak in healthcare, despite both being high-consequence domains? Is it regulatory structure, cultural commitment to safety, or economic incentives?',
    'Institutional historical analysis: trace the founding of aviation safety investigations versus medical error reporting; examine whether legal liability structures, professional culture, or regulatory mandates drive the difference.',
    'If the difference is regulatory mandate (FAA requires participation in ASRS; no medical equivalent), the constraint is contingent and could be extended. If it is professional culture or economic incentive structure, extending incident-sharing to healthcare would require cultural shift or incentive realignment. The answer determines whether the sibling reading (catastrophe_as_necessary_selector) is actually describing a structural necessity or just the outcome of regulatory weakness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incident_access_exclusion_mechanism, conceptual, 'Why incident-sharing infrastructure varies across high-reliability domains.').

omega_variable(
    regulatory_capture_in_incident_networks,
    'Do safety authorities benefit from maintaining the scarcity of incident access (strong gatekeeping, slow corrective cycles) because it preserves their legitimacy and administrative infrastructure, rather than optimizing for practitioner competence?',
    'Analyze corrective action timelines: measure how quickly incidents cascade from discovery to mandatory corrective action across different regulatory regimes. Compare against what technological constraints would suggest (communication speed, analysis capacity). Look for systematic delays that benefit authorities more than practitioners.',
    'If capture is present, the theater_ratio will continue rising and the constraint will evolve into a piton: maintained by performance rather than function, benefiting authorities more than practitioners. If capture is absent, the constraint''s extraction is legitimately the cost of coordination rather than institutional rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_in_incident_networks, empirical, 'Whether incident-sharing authorities optimize for learning or for institutional preservation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t7, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 7, 0.19).
narrative_ontology:measurement_basis(cata_tr_t7, observed).
narrative_ontology:measurement(cata_tr_t14, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 14, 0.23).
narrative_ontology:measurement_basis(cata_tr_t14, observed).
narrative_ontology:measurement(cata_tr_t21, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 21, 0.27).
narrative_ontology:measurement_basis(cata_tr_t21, observed).
narrative_ontology:measurement(cata_tr_t35, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 35, 0.3).
narrative_ontology:measurement_basis(cata_tr_t35, observed).
narrative_ontology:measurement(cata_tr_t50, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 50, 0.31).
narrative_ontology:measurement_basis(cata_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t7, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 7, 0.42).
narrative_ontology:measurement_basis(cata_be_t7, observed).
narrative_ontology:measurement(cata_be_t14, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 14, 0.48).
narrative_ontology:measurement_basis(cata_be_t14, observed).
narrative_ontology:measurement(cata_be_t21, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 21, 0.54).
narrative_ontology:measurement_basis(cata_be_t21, observed).
narrative_ontology:measurement(cata_be_t35, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 35, 0.57).
narrative_ontology:measurement_basis(cata_be_t35, observed).
narrative_ontology:measurement(cata_be_t50, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(cata_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t7, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 7, 0.32).
narrative_ontology:measurement_basis(cata_su_t7, observed).
narrative_ontology:measurement(cata_su_t14, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 14, 0.36).
narrative_ontology:measurement_basis(cata_su_t14, observed).
narrative_ontology:measurement(cata_su_t21, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 21, 0.39).
narrative_ontology:measurement_basis(cata_su_t21, observed).
narrative_ontology:measurement(cata_su_t35, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 35, 0.41).
narrative_ontology:measurement_basis(cata_su_t35, observed).
narrative_ontology:measurement(cata_su_t50, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 50, 0.42).
narrative_ontology:measurement_basis(cata_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__hybrid_near_miss_learning, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.12).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_as_proxy_catastrophe).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe-avoidance-retention kernel. The sibling readings (catastrophe_as_necessary_selector, simulation_as_proxy_catastrophe) instantiate structurally distinct claims about what maintains competence: real catastrophes only, versus simulation, versus distributed near-miss learning. Each reading has a different ε (measured extractiveness), different beneficiary/victim structure, and different core axiom. This story (hybrid_near_miss_learning) claims ε ≈ 0.58 with extractiveness rising over time as regulatory authority consolidates; it carries the axiom that NEITHER simulation alone NOR catastrophe alone suffices. The sibling simulation_as_proxy_catastrophe carries the axiom that simulation DOES suffice, and would measure lower extraction if practitioner autonomy is preserved through optional drill participation. The sibling catastrophe_as_necessary_selector carries the axiom that actual catastrophe provides irreplaceable selection pressure, and would measure competence degradation in domains without it regardless of incident-sharing. All three stories are empirically contestable and structurally distinct; they are not the same constraint viewed from different angles. The network links show mutual influence: stronger incident-sharing networks (this reading) displace the necessity for catastrophic selection (sibling 1) but do not fully displace the need for high-realism drills (sibling 2).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_avoidance_retention__hybrid_near_miss_learning, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
