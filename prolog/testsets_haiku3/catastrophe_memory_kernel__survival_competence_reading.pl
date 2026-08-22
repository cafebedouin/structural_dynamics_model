% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Catastrophe Memory Kernel — Survival Competence Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint is the SURVIVAL COMPETENCE reading of the contested
 *   catastrophe_memory_kernel. The kernel is a persecuted community's
 *   ritualized mourning-practice and collective memory encoded in enacted
 *   tradition. This reading frames ritual as a mechanism for transmitting
 *   adaptive capacity — specific operational knowledge for
 *   persecution-response — across generations. The reading differs from
 *   sibling readings: boundary_maintenance_reading emphasizes group-identity
 *   enforcement; symbol_continuity_reading emphasizes identity preservation
 *   across time; trauma_encoding_reading emphasizes intergenerational trauma
 *   as warning system. This reading's distinctive focus is on ritual as
 *   survival-training: knowledge-encoding and rehearsal of
 *   catastrophe-response patterns. The constraint is CLAIMED as tangled_rope
 *   (genuine coordination in survival-knowledge transmission + asymmetric
 *   extraction via boundary-maintenance costs) and the metrics descriptively
 *   reflect that: moderate extractiveness, lower suppression (the ritual is
 *   internally enforced, not externally coerced), low theater (the
 *   survival-competence function is operational, though dormant in safe
 *   periods). The measurement series tracks the trajectory over 100
 *   time-units, modeling both safe periods (t=28–70, low extractiveness) and
 *   periods of heightened threat activation (t=42–70, rising theater as
 *   historical memory becomes operative survival-guidance). The dip at t=100
 *   reflects post-crisis normalization when the acute threat passes.
 *
 * KEY AGENTS:
 *   - persecuted_community: collective beneficiary — receives resilience capacity encoded in ritual rehearsal
 *   - ritual_authority: agenda-setter — maintains canon, enforces participation, claims interpretive authority over survival-knowledge
 *   - community_members_assimilating: primary payers — bear boundary-maintenance cost and reduced mobility
 *   - younger_generation: secondary payers — learn ritual whose survival-relevance is historically attenuated until threat returns
 *   - oppressor_regime: structurally excluded — would decode and suppress if given interpretive access
 *   - diaspora_scholars: analytical observers — validate survival-competence function through external documentation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.48).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.31).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Catastrophe Memory Kernel — Survival Competence Reading").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, '7c7e62a2-d68e-4dc8-a5c5-ca05f663c618').
narrative_ontology:cs_kernel_codification('7c7e62a2-d68e-4dc8-a5c5-ca05f663c618', distributed).
narrative_ontology:cs_authority_grounding('7c7e62a2-d68e-4dc8-a5c5-ca05f663c618', practice).
narrative_ontology:cs_interpretation_layer_present('7c7e62a2-d68e-4dc8-a5c5-ca05f663c618').
narrative_ontology:cs_reading_relation('7c7e62a2-d68e-4dc8-a5c5-ca05f663c618', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c7e62a2-d68e-4dc8-a5c5-ca05f663c618', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c7e62a2-d68e-4dc8-a5c5-ca05f663c618', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_axiom('7c7e62a2-d68e-4dc8-a5c5-ca05f663c618', foundational, ritual_transmits_operational_survival_knowledge).
narrative_ontology:cs_axiom_status(ritual_transmits_operational_survival_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('7c7e62a2-d68e-4dc8-a5c5-ca05f663c618', ritual_transmits_operational_survival_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('7c7e62a2-d68e-4dc8-a5c5-ca05f663c618', foundational, collective_resilience_justifies_boundary_maintenance_costs).
narrative_ontology:cs_axiom_status(collective_resilience_justifies_boundary_maintenance_costs, holdable).
narrative_ontology:cs_axiom_grounding('7c7e62a2-d68e-4dc8-a5c5-ca05f663c618', collective_resilience_justifies_boundary_maintenance_costs, instrumental).
narrative_ontology:cs_reference_frame('7c7e62a2-d68e-4dc8-a5c5-ca05f663c618', persecution_cycle_recurrence_framework).
narrative_ontology:cs_drift_state('7c7e62a2-d68e-4dc8-a5c5-ca05f663c618', contemporary_diaspora_safety_periods, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7c7e62a2-d68e-4dc8-a5c5-ca05f663c618', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, persecuted_community).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, community_members_assimilating).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, younger_generation).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, younger_generation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in and transmits ritualized mourning-practices that encode survival knowledge: how to hide, maintain networks, preserve literacy, recognize threats, respond to violence. Ritual rehearsal maintains operational readiness for persecution cycles. The community reproduces itself through these practices and derives resilience capacity from their enactment.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, persecuted_community, beneficiary,
    organized, generational, identity_locked, global).

% Codifies, interprets, and enforces the ritual canon — which practices are canonical, how they must be performed, who may lead them, what knowledge is embedded in each movement or recitation. Maintains interpretive authority over the kernel catastrophe-memory and its adaptive content. Justifies enforcement as fidelity to tradition and community survival.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, ritual_authority, agenda_setter,
    institutional, generational, constrained, global).

% Face pressure to participate in rituals that explicitly mark group boundaries and encode group-specific threat recognition — practices that make integration into host societies harder, that signal 'otherness' to outsiders, that constrain individual mobility. Assimilation offers safety through invisibility; ritual participation forecloses that exit. They bear the boundary-maintenance cost that survival competence requires.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, community_members_assimilating, payer,
    moderate, biographical, identity_locked, global).

% Required to learn and perform rituals whose direct survival relevance is attenuated or historical — in periods of safety or low-salience threat, the rehearsal of catastrophe-response feels like performative burden rather than necessary skill. Yet the ritual transmission IS the mechanism through which competence is preserved for the cycle when threat returns. They bear learning costs to inherit adaptive capacity they may not immediately need.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, younger_generation, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__survival_competence_reading, younger_generation, beneficiary).

% Excluded from understanding or legitimating the ritual structure; would recognize it, if decoded, as a survival-training system and would move to suppress it. The ritual's opacity is structural to its function — persecution-adapted communities encode survival knowledge in forms that resist hostile decoding.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, oppressor_regime, excluded,
    institutional, biographical, trapped, global).

% Document and analyze the ritual system from outside — examining which encoded practices correlate with survival outcomes, which have degraded or been forgotten, how threat cycles activate dormant practice-knowledge. Provide external corroboration of the survival-competence function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, diaspora_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__survival_competence_reading, ritual_authority).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits collective knowledge of persecution-response across generations: communication networks, resource-hiding, threat recognition, coordinated escape, identity-protection, document-preservation. Ritual creates a compressed, durable, hard-to-disrupt transmission channel for operational competence that would be lethal if encoded in writing or direct speech.
% TRANSFER_FUNCTION: Moves individual assimilation-readiness (ease of passing, reduced boundary-visibility, lower social cost of participation) from community members who learn ritual to the community collective, which gains preserved survival competence. Community members pay the cost of visible otherness; the community collects resilience capacity.
% ABSENT_VOICES: Members of host societies who would benefit from open assimilation pathways if ritual-boundary-maintenance were relaxed; younger community members who, in safe periods, might strategically assimilate and would prefer choice over pre-commitment. Both are structurally excluded from decision-making about ritual continuity — the ritual apparatus itself pre-commits the community against that dissolution.
% DISAPPEARANCE_RATIONALE: If the ritual and its enforcement vanished, persecuted communities would face a knowledge-collapse: the distributed survival competence would be lost, encoded only in dispersed individual memory, vulnerable to diaspora, decay, or hostile decoding. The next persecution cycle would find the community unprepared. The community's survival would rearrange around lower resilience and higher casualty rates. Communities that abandoned ritual in periods of safety would rediscover need for it when threat returned, having lost both competence and institutional memory of how to transmit it.
% FOUNDING_PROBLEM: Persecution creates recurrent cycles of threat and hiding. Communities must preserve response-competence across generations, including generations born in relative safety when direct threat experience is absent. Writing preservation is too dangerous; oral rehearsal is too fragile to non-initiated ears. Ritualized enactment creates a durable, resistant, reproducible transmission channel.
% FOUNDING_PROBLEM_CORROBORATION: Diaspora historians, ethnographers of persecution-adapted communities, and communities themselves attest that persecution cycles remain live and that ritualized knowledge-transmission enabled survival in prior cycles. Oppressive regimes' historical attempts to suppress ritual — recognizing its threat-encoding function — confirm the founding problem persists as long as persecution risk exists. Comparative documentation of diaspora communities shows those maintaining ritual practice show higher preservation of survival-competence; those abandoning ritual show rapid knowledge degradation.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because ritual carries both a genuine coordination function (preserving survival-knowledge) and an asymmetric cost (individual boundary-maintenance requirements). The constraint exhibits classic tangled-rope structure: the coordination benefit is real and non-zero (the alternative to ritual transmission is knowledge loss and vulnerability); the extraction is real and non-zero (members who would assimilate and thereby reduce their own persecution-risk are pre-committed by participation). Suppression is lower than extractiveness (0.31) because the enforcement is primarily internal (community enforcement of ritual participation) rather than external coercion — members identify with the community and accept boundary costs; the constraint is not held in place by fear alone. Theater ratio is low (0.22) because the survival-competence function is operational and observable — during threat periods, the encoded knowledge becomes directly actionable; even in safe periods, the ritual rehearsal is instrumentally oriented toward competence maintenance, not toward performative legitimation. The measurement series captures a cyclical dynamic: baseline extractiveness in safe periods (t=0–28), rising extractiveness and theater as historical memory activates (t=28–70, corresponding to periods of heightened persecution risk or commemoration intensity), then partial retreat as acute crisis passes (t=70–100, returning to baseline but not to pre-crisis levels as institutional memory hardens). The shared time grid ensures every metric is authored at every point; absence of a metric at a point would inject end-state values at earlier times and distort temporal inference (OQ-105 guard).
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute substantially different types: from the persecuted_community seat, the arrangement is tangled_rope or even rope (coordination carrying moderate cost, but essential). From the assimilating_member seat, the arrangement approaches snare (boundary-enforcement with limited exit, limited beneficiary status for those individuals). From the ritual_authority seat, the arrangement is rope or scaffold (temporary support for a community in crisis, with the mandate that it transitions away if threat subsides — but mandatrophy may resolve if the ritual persists beyond active persecution as institutional identity). The engine computes these divergences; the authored claim (tangled_rope) represents the most accurate constraint-level type but does not predict all seat-level types.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is NOT uniform across the constraint. The persecuted_community as a collective benefits from resilience capacity: their d is low (near 0.0) because they are net beneficiaries and power is moderate. Individual members who are assimilating-oriented face higher d (near 0.7–0.8) because they are targeted by boundary-maintenance enforcement and identity_locked exit amplifies the constraint's hold. Ritual_authority sits near symmetric (d ≈ 0.5) because they both administer the constraint (beneficiary position) and bear enforcement costs (payer position). The younger_generation bears learning costs and lost assimilation-readiness but inherits resilience capacity — their d depends on how threat cycles align with their biographical horizon; in safe periods, d rises (bearing learning cost with no immediate benefit); in threat periods, d drops (learning cost is repaid by operative survival-knowledge). These divergences across seats are derived from the structural data (power, exit, beneficiary/victim declarations) without override; no directionality_overrides are needed because the derivation chain produces accurate seat differentiation from power + exit + role.
 *
 * MANDATROPHY ANALYSIS:
 *   The survival-competence reading carries mandatrophy risk: IF persecution cycles end and the threat landscape shifts to genuine safety, the mandate (preserve survival-competence for persecution-response) may outlive its function. Communities historically confronted this problem: ritual may persist as boundary-maintenance mechanism or as cultural identity-marker after its survival function attenuates. This reading's omega variables address whether the constraint represents live adaptation or (in safe periods) theatrical maintenance of a once-functional form. The measurement series at t=100 shows extractiveness declining from peak but NOT returning to t=0 baseline, suggesting institutional inertia: the ritual persists at some cost even after acute threat passes. The founding_problem_status is declared 'live' because persecution cycles DO persist globally; but for specific communities in specific safe periods, the founding problem may be dormant, triggering mandatrophy diagnostic signals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    live_threat_vs_historical_trauma,
    'Is the survival-competence function operative (persecution cycles are live and recurring) or historical (trauma is being transmitted and rehearsed, but acute persecution is not currently active)?',
    'Empirical assessment: threat-environment analysis for the specific community; measurement of whether ritual rehearsal maps to active threat-response or to memorial/identity-maintenance.',
    'If live threat, extractiveness is justified by coordination necessity and the constraint is tangled_rope. If historical, the constraint may be mandatrophy candidate (ritual persisting as identity-marker after survival-function has attenuated) or may be transitioned to pure identity_coordination type. Threat-activation changes the constraint''s classification across seats and over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(live_threat_vs_historical_trauma, empirical, 'Whether survival-competence function is operationally active or historically encoded.').

omega_variable(
    ritual_encoding_fidelity,
    'How much of the encoded survival-knowledge is accessible to new generations without community membership or apprenticeship? Is the encoding robust against diaspora, generational drift, or hostile decoding?',
    'Documentation of ritual-knowledge transmission failures, diaspora community data on ritual-practice degradation, historical records of ritual-suppression effectiveness.',
    'If encoding is robust, the ritual is a reliable knowledge-transmission mechanism and the constraint''s survival-coordination function is high-fidelity. If encoding degrades or is easily disrupted, the constraint''s protective function is weaker and extractiveness may not be justified by coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_encoding_fidelity, empirical, 'Robustness of ritual-encoded survival-knowledge across diaspora and time.').

omega_variable(
    identity_fusion_vs_instrumental_participation,
    'Do community members participate in ritual because they identify with the group and accept boundary costs (identity fusion) or because they are coerced or socialized into participation despite preferring assimilation (internalized suppression)?',
    'Post-exit behavior: if members who leave the community abandon the ritual, identity was fused; if they retain ritual practice or struggle to abandon it, suppression was internalized. Ethnographic observation of motivation heterogeneity.',
    'If fusion is primary, suppression metric is lower (coercion is minimal) and constraint is higher-functioning tangled_rope. If internalized suppression is primary, the constraint carries hidden extraction: members comply but under internalized duress, and the extraction is higher than suppression metric alone captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_vs_instrumental_participation, empirical, 'Whether boundary-maintenance participation is identity-fused or internalized-suppression-driven.').

omega_variable(
    sibling_reading_empirical_divergence,
    'Can the survival_competence reading be empirically distinguished from the trauma_encoding reading? Do they make falsifiable different predictions about ritual structure or transmission outcomes?',
    'Comparative ethnography: survival_competence predicts ritual will encode specific operational knowledge (routes, signals, resource-caches); trauma_encoding predicts ritual will emphasize emotional/memorial content. Empirical audit of ritual content and transmitted knowledge.',
    'If empirically distinguishable, they are genuinely two constraints (epsilon-invariance principle). If empirically identical, they are two interpretations of one constraint and should be unified (kernel-reading relationship confirmed). This affects network.affects_constraints and whether separate JSON files are warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_empirical_divergence, empirical, 'Whether survival_competence and trauma_encoding readings are empirically distinct or interpretively identical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(cata_tr_t0, projected).
narrative_ontology:measurement(cata_tr_t14, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 14, 0.12).
narrative_ontology:measurement_basis(cata_tr_t14, projected).
narrative_ontology:measurement(cata_tr_t28, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 28, 0.16).
narrative_ontology:measurement_basis(cata_tr_t28, observed).
narrative_ontology:measurement(cata_tr_t42, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 42, 0.21).
narrative_ontology:measurement_basis(cata_tr_t42, observed).
narrative_ontology:measurement(cata_tr_t70, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 70, 0.28).
narrative_ontology:measurement_basis(cata_tr_t70, observed).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement_basis(cata_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(cata_be_t0, projected).
narrative_ontology:measurement(cata_be_t14, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 14, 0.44).
narrative_ontology:measurement_basis(cata_be_t14, projected).
narrative_ontology:measurement(cata_be_t28, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 28, 0.46).
narrative_ontology:measurement_basis(cata_be_t28, observed).
narrative_ontology:measurement(cata_be_t42, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 42, 0.49).
narrative_ontology:measurement_basis(cata_be_t42, observed).
narrative_ontology:measurement(cata_be_t70, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 70, 0.52).
narrative_ontology:measurement_basis(cata_be_t70, observed).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement_basis(cata_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(cata_su_t0, projected).
narrative_ontology:measurement(cata_su_t14, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 14, 0.25).
narrative_ontology:measurement_basis(cata_su_t14, projected).
narrative_ontology:measurement(cata_su_t28, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 28, 0.28).
narrative_ontology:measurement_basis(cata_su_t28, observed).
narrative_ontology:measurement(cata_su_t42, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 42, 0.31).
narrative_ontology:measurement_basis(cata_su_t42, observed).
narrative_ontology:measurement(cata_su_t70, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 70, 0.35).
narrative_ontology:measurement_basis(cata_su_t70, observed).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 100, 0.31).
narrative_ontology:measurement_basis(cata_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__survival_competence_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested catastrophe_memory_kernel. All four readings (survival_competence, boundary_maintenance, symbol_continuity, trauma_encoding) share the same standing ritual practice as their referent, but propose different primary functions. The survival_competence reading is distinguished by its claim that the primary function is encoding and transmitting operational knowledge for persecution-response catastrophe-adaptation. This reading coexists with sibling readings as interpretive frameworks held by different seats (scholars, communities, authorities) and is not foreclosed by any sibling's core premise — they are mutually compatible interpretations of complex ritual function. The network links are bidirectional to enable contamination-analysis across readings; a degradation in one reading's empirical support (e.g., evidence that ritual-encoded knowledge is not operationally accessible) may influence the validity of adjacent readings (e.g., shifting emphasis from survival_competence to trauma_encoding as primary function).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
