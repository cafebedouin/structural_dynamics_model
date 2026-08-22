% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__husk_reading, []).

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
 *   constraint_id: preparedness_transmission__husk_reading
 *   human_readable: Preparedness Transmission as Memorial Ritual (Husk Reading)
 *   domain: institutional/disaster_risk_management
 *
 * SUMMARY:
 *   A national civil defense system maintains annual drills, inspection
 *   protocols, and certification standards for emergency responders. The
 *   system's institutional authority rests on the claim that these rituals
 *   transmit and validate operational preparedness knowledge across
 *   generational turnover. In the husk reading, the system persists through
 *   protocol compliance but the underlying adaptive knowledge has decayed.
 *   Drills and inspections detect only pre-specified failure modes; when
 *   novel flood scenarios occur, the system's brittleness is revealed. The
 *   institutional apparatus benefits from continued ritual performance
 *   because admission of knowledge decay would threaten its authority and
 *   funding; responders and flood-exposed communities bear the cost of the
 *   mismatch between protocol form and actual capability. This reading claims
 *   the constraint is a Piton: a former coordination mechanism (genuine
 *   knowledge transmission) that now persists primarily through institutional
 *   inertia and theatrical maintenance, with minimal adaptive capability.
 *
 * KEY AGENTS:
 *   - Institutional Continuity Apparatus: administers the formal system, sets protocol, certifies compliance (power=institutional, exit=mobile)
 *   - Trained Emergency Responders: execute drills and inspections, perform in actual incidents, experience protocol-capability gap (power=organized, exit=constrained)
 *   - Flood-Exposed Communities: depend on the system for protection, face hidden brittleness in novel scenarios (power=powerless, exit=trapped)
 *   - Audit/Inspection Bodies: certify protocol compliance without validating operational capability (power=institutional, exit=mobile)
 *   - Competing Knowledge Systems: excluded alternative expertise holders (hydrologists, climate scientists, community practitioners) (power=moderate, exit=constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.62).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.48).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Preparedness Transmission as Memorial Ritual (Husk Reading)").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "institutional/disaster_risk_management").

domain_priors:requires_active_enforcement(preparedness_transmission__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, 'f6fb1292-a252-4f2d-93b5-b599419050ce').
narrative_ontology:cs_kernel_codification('f6fb1292-a252-4f2d-93b5-b599419050ce', formalized).
narrative_ontology:cs_authority_grounding('f6fb1292-a252-4f2d-93b5-b599419050ce', extraction).
narrative_ontology:cs_interpretation_layer_present('f6fb1292-a252-4f2d-93b5-b599419050ce').
narrative_ontology:cs_reading_relation('f6fb1292-a252-4f2d-93b5-b599419050ce', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6fb1292-a252-4f2d-93b5-b599419050ce', preparedness_transmission__hybrid_reading, influences).
narrative_ontology:cs_axiom('f6fb1292-a252-4f2d-93b5-b599419050ce', foundational, protocol_form_decoupled_from_adaptive_capability).
narrative_ontology:cs_axiom_status(protocol_form_decoupled_from_adaptive_capability, holdable).
narrative_ontology:cs_axiom_grounding('f6fb1292-a252-4f2d-93b5-b599419050ce', protocol_form_decoupled_from_adaptive_capability, empirically_contingent).
narrative_ontology:cs_axiom('f6fb1292-a252-4f2d-93b5-b599419050ce', secondary, ritual_performance_sustains_institutional_authority).
narrative_ontology:cs_axiom_status(ritual_performance_sustains_institutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('f6fb1292-a252-4f2d-93b5-b599419050ce', ritual_performance_sustains_institutional_authority, deontological).
narrative_ontology:cs_reference_frame('f6fb1292-a252-4f2d-93b5-b599419050ce', knowledge_transmission_through_codified_protocol).
narrative_ontology:cs_drift_state('f6fb1292-a252-4f2d-93b5-b599419050ce', contemporary_flood_scenario_diversity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f6fb1292-a252-4f2d-93b5-b599419050ce', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, institutional_continuity_apparatus).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, civil_defense_bureaucracy).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, flood_exposed_communities).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, novel_scenario_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, audit_and_inspection_bodies).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, trained_emergency_responders).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, successor_generation_trainees).
narrative_ontology:constraint_vindicates(preparedness_transmission__husk_reading, institutional_memory_persistence_doctrine).
narrative_ontology:constraint_vindicates(preparedness_transmission__husk_reading, ritual_performance_as_legitimacy_anchor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the civil defense preparedness system, conducts annual drills, certifies inspection compliance, and maintains the bureaucratic infrastructure. Justifies the system as continuity of essential knowledge and capability. Benefits from the continuation of the apparatus itself — budgets, personnel continuity, institutional authority — regardless of whether the knowledge remains operationally current. Could redesign the system but the cost of admission (acknowledgment that preparedness has decayed) exceeds the institution's tolerance.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, institutional_continuity_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% Execute drills and inspections per protocol, maintain certification status, and respond to actual incidents. They experience the gap between protocol form (which is routinely practiced and complied with) and novel scenario performance (where the system fails because the underlying knowledge has atrophied). Their professional identity and career advancement are invested in the institutional continuity apparatus; exiting means leaving the profession.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, trained_emergency_responders, payer,
    organized, biographical, constrained, regional).

% Depend on the civil defense system to function in a flood emergency. They are presented with an apparatus that claims readiness but is actually calibrated to detect only pre-specified failure modes. When a novel flood scenario occurs (magnitude deviation, timing shift, compound hydrology), the system's hidden brittleness is revealed. They cannot exit the constraint; they are its primary target.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, flood_exposed_communities, payer,
    powerless, biographical, trapped, local).

% Certify institutional compliance with protocol and inspect drill execution. The institutional continuity apparatus funds their work and accepts their certifications without independent validation of operational capability. They benefit from continued protocol-based inspection because it sustains their authority and funding; actual performance validation would require different expertise and would expose them to liability.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, audit_and_inspection_bodies, beneficiary,
    institutional, generational, mobile, national).

% Called upon during incidents that deviate from the pre-specified scenarios the system was trained to handle. They face a collision: the institutional memory system asserts readiness through protocol compliance, but the adaptive knowledge needed for novel scenarios was never transmitted or has decayed. Professional identity as 'part of the system' prevents exit; the constraint locks them into performing their role with degraded capability.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, novel_scenario_responders, payer,
    moderate, immediate, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__husk_reading, novel_scenario_responders, observer).

% Independent hydrologists, climate scientists, and community-based disaster management practitioners who hold knowledge about emerging flood patterns and adaptation strategies. They are not integrated into the civil defense apparatus; their knowledge is treated as external or redundant. Their exclusion sustains the fiction that the formal system is sufficient.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, competing_knowledge_systems, excluded,
    moderate, biographical, constrained, regional).

% Trained through the formal curriculum and certification pathway, learning the protocol form without exposure to the adaptive knowledge that would enable performance in novel scenarios. They inherit a system that performs membership in a competence community without access to the underlying competence. Their only exit path is abandoning the profession entirely, which represents both economic loss and severing identity-constitutive relationships.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, successor_generation_trainees, payer,
    powerless, biographical, identity_locked, national).

% Assesses the constraint structure: a system that persists through ritual performance and institutional continuity while the underlying adaptive knowledge erodes. The observer sees the theatrical infrastructure and the actual brittleness it masks.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, analyst_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__husk_reading, institutional_continuity_apparatus).
narrative_ontology:fixing_cost_class(preparedness_transmission__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains organizational continuity of emergency response roles and institutional authority; transmits role-based procedural knowledge through certification and drill performance; coordinates distributed response infrastructure against standardized protocol.
% TRANSFER_FUNCTION: Moves authority, legitimacy, and resource allocation from the adaptive knowledge system (which has decayed) to the ritual performance system (which persists). Extracts compliance and institutional deference from responders and flood-exposed communities in exchange for a preparedness apparatus that is opaque about its brittleness.
% ABSENT_VOICES: Competing knowledge holders (independent hydrologists, climate scientists, community disaster practitioners) are structurally excluded from the apparatus; they would attest that pre-specified scenarios are no longer predictive of actual flood behavior. Successor-generation responders who experience the gap between protocol form and actual capability are kept within the institutional frame where that gap cannot be articulated without professional risk.
% DISAPPEARANCE_RATIONALE: If the formal civil defense apparatus and its ritual protocol disappeared, flood communities would organize disaster response through alternative knowledge networks, community-based early warning, and adaptive learning from actual incidents. Responders would re-credential through competence-based training rather than protocol certification. The institutional continuity apparatus would lose its administrative function and funding stream.
% FOUNDING_PROBLEM: After a major historical flood disaster, the institutional response was to codify the response protocols that had worked, establish certification standards, and mandate periodic drills to ensure knowledge transmission across generations. The founding problem was: how do we ensure the knowledge that saved us in crisis is not lost to organizational turnover?
% FOUNDING_PROBLEM_CORROBORATION: The institutional continuity apparatus attests the founding problem remains live, citing the ongoing necessity of preparedness. Independent emergency management researchers and post-incident after-action reviews attest that the founding problem—knowledge loss through generational turnover—was solved by early codification and drill practice, but that the constraint has persisted past its functional life as a legitimacy anchor rather than a competence mechanism. The gap between protocol compliance and actual performance in recent novel flood scenarios corroborates the status=dead reading.
narrative_ontology:disappearance_verdict(preparedness_transmission__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__husk_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory shows initial rise (0.38→0.62 over 25 time points) as the gap between protocol form and actual capability widens, then plateau (0.62 constant t=25→40) once the system has stabilized at high theater-ratio performance. Theater ratio rises sharply (0.35→0.71 over 25 time points) as more of the system's activity becomes decoupled from adaptive knowledge transmission and serves primarily to maintain institutional legitimacy and authority. Suppression remains modest (0.48 endpoint) because the constraint persists largely through institutional inertia and lack of viable alternative organization rather than active coercive enforcement. Accessibility collapse is low (0.38) because responders and communities maintain potential exit paths (alternative disaster response networks, competence-based retaining) even though identity-lock and institutional capture make exits costly. Resistance is moderate (0.55) because the constraint faces real pressure from recent flood incidents that exposed its brittleness, yet the institutional apparatus successfully absorbs criticism by incrementally adjusting protocol without admitting underlying knowledge decay.
 *
 * PERSPECTIVAL GAP:
 *   The institutional continuity apparatus experiences the constraint as legitimate coordination — the system works as designed, drills are complied with, certifications are current — and sees no reason to change. Trained responders experience a professional gap: they know the protocol and can execute it flawlessly, but they also experience the constraint's hidden brittleness when actual incidents deviate from the pre-specified scenarios. Flood-exposed communities experience the constraint as pure extraction: they are presented with claims of preparedness that the system itself cannot substantiate, and they bear the costs when the constraint fails. The engine computes these seat-specific types from the structural data — the husk reading authorship makes the asymmetry explicit.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional continuity apparatus is the structural beneficiary: it maintains authority, budgets, and legitimacy through continued protocol performance. Directionality d for this agent is low (~0.1-0.2), indicating subsidy/benefit. Trained responders sit near the payer end (d~0.65-0.75): they are constrained into certification and drill participation, they experience professional risk if they articulate the knowledge-capability gap, and they bear the cost of identity-lock that prevents exit. Flood-exposed communities sit at the target end (d~0.85-0.95): they are trapped geographically, they depend on the apparatus for protection, and they bear the cost of hidden brittleness when the system fails. Novel scenario responders are identity-locked (the constraint defines what it means to be 'competent' in the formal system) and sit near payer end (d~0.7-0.8). The audit/inspection bodies benefit from the system's continuation and sit near beneficiary end (d~0.15-0.25).
 *
 * MANDATROPHY ANALYSIS:
 *   The husk reading resolves mandatrophy by documenting that the constraint's founding mandate (transmit knowledge across generational turnover) has been supplanted by institutional persistence and authority maintenance. The system was founded to solve a real coordination problem: how to preserve response capability when individuals retire. That problem was solved by early codification and drill practice. But the constraint persists past its functional life because admission that the underlying knowledge has decayed would threaten the institutional apparatus's authority and funding. The theater ratio trajectory (rising from 0.35 to 0.71 and then plateau) documents the point at which ritual performance came to dominate actual function transmission. The founding_problem_status=dead with disappearance_verdict=world_rearranges confirms that the constraint is a zombie coordination mechanism: it persists through inertia and institutional capture despite the founding mandate being satisfied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    knowledge_decay_mechanism_ambiguity,
    'Is the measured knowledge decay a structural property of time and generational turnover (knowledge loss is inevitable, and the system is failing to transmit adequately), or is it a product of how knowledge is codified and transmitted (the constraint optimized for protocol form at the expense of adaptive reasoning)?',
    'Comparative analysis across organizations with different knowledge transmission models (centralized protocol vs. apprenticeship-based adaptive training). Post-incident analysis of responder decision-making under novel conditions vs. protocol predictions.',
    'If structural property: the constraint faces an irreducible knowledge-loss problem and any reorganization would face the same challenge. If product of codification: the constraint''s method of transmission is the problem, not the constraint itself; alternative transmission methods could restore knowledge transmission without abandoning the institutional apparatus. The reading claims the latter (the system chose shallow codification over adaptive transmission).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_decay_mechanism_ambiguity, empirical, 'Whether knowledge decay is inevitable or a product of the constraint''s transmission method.').

omega_variable(
    theater_detection_sensitivity,
    'Is the rising theater_ratio trajectory an accurate measurement of performative activity displacing functional knowledge transmission, or does it reflect increased administrative documentation (which may be theater-like in form but is still functionally integrated)?',
    'Time-use analysis of responder activity in drills vs. novel incidents; content analysis of drill scenarios showing the proportion pre-specified vs. novel elements over time; interviews with responders about learning outcomes vs. protocol compliance.',
    'If theater_ratio rise reflects genuine performativity: the Piton classification is correct. If it reflects administrative intensification with real functional integration: the constraint may be more competence-preserving than the husk reading claims. The husk reading assumes the former.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_detection_sensitivity, empirical, 'Whether rising theater_ratio reflects actual knowledge-capability gap or administrative intensification.').

omega_variable(
    suppression_mechanism_in_responder_identity_lock,
    'The measured suppression (0.48) is modest; responders are not overtly coerced to comply with drills. Is the suppression structural (external barriers to exit) or primarily internalized (professional identity has fused with the constraint such that exit feels impossible despite low external barriers)?',
    'Post-exit trajectory analysis: when responders leave the system, does suppression persist? If post-exit suppression is high (they continue to internalize the constraint''s legitimacy claims after leaving), the mechanism is partly internalized. If post-exit suppression collapses quickly, it is mostly structural.',
    'If internalized: the constraint''s effective suppression may be higher than the 0.48 metric suggests, and responders carry the suppression with them after exit. If structural: exit barriers are institutional (career, identity, social positioning) but not psychological, and alternative organization would be feasible with lower suppression cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_in_responder_identity_lock, empirical, 'Is suppression in preparedness constraint structural or internalized in responder identity?').

omega_variable(
    kernel_reading_decomposition,
    'Is the husk_reading a genuine alternative reading of the preparedness_transmission kernel, or does it collapse back to the competence_reading when pressed on specific competencies (e.g., ''is the system adequate for pre-specified scenarios'')? Where exactly is the reading boundary?',
    'Specification of which competencies the husk reading claims are transmitted (e.g., ''protocol execution, role-based coordination''), which are not (e.g., ''adaptive reasoning in novel scenarios''), and which are contested between readings (e.g., ''magnitude-adjusted hydrology''). Detailed comparison of post-incident performance for pre-specified vs. novel scenarios across responder cohorts trained in husk vs. competence vs. hybrid frameworks.',
    'If the readings are genuinely decomposable on competency dimension: the husk_reading is a coherent alternative with different ε, different victim sets, and different classification. If the husk_reading merely asserts ''some competencies are missing'' without specifying which the constraint successfully transmits, the reading collapses into the competence_reading at higher resolution. The husk_reading authors assume genuine decomposition on the adaptive-reasoning axis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Kernel reading decomposition: husk_reading vs. competence_reading boundary on competency transmission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__husk_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t5, preparedness_transmission__husk_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement_basis(prep_tr_t5, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__husk_reading, theater_ratio, 10, 0.51).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t15, preparedness_transmission__husk_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement_basis(prep_tr_t15, observed).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__husk_reading, theater_ratio, 20, 0.64).
narrative_ontology:measurement_basis(prep_tr_t20, observed).
narrative_ontology:measurement(prep_tr_t25, preparedness_transmission__husk_reading, theater_ratio, 25, 0.68).
narrative_ontology:measurement_basis(prep_tr_t25, observed).
narrative_ontology:measurement(prep_tr_t30, preparedness_transmission__husk_reading, theater_ratio, 30, 0.71).
narrative_ontology:measurement_basis(prep_tr_t30, observed).
narrative_ontology:measurement(prep_tr_t35, preparedness_transmission__husk_reading, theater_ratio, 35, 0.71).
narrative_ontology:measurement_basis(prep_tr_t35, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__husk_reading, theater_ratio, 40, 0.71).
narrative_ontology:measurement_basis(prep_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__husk_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t5, preparedness_transmission__husk_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(prep_be_t5, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__husk_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t15, preparedness_transmission__husk_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement_basis(prep_be_t15, observed).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__husk_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(prep_be_t20, observed).
narrative_ontology:measurement(prep_be_t25, preparedness_transmission__husk_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(prep_be_t25, observed).
narrative_ontology:measurement(prep_be_t30, preparedness_transmission__husk_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(prep_be_t30, observed).
narrative_ontology:measurement(prep_be_t35, preparedness_transmission__husk_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement_basis(prep_be_t35, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__husk_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(prep_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__husk_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t5, preparedness_transmission__husk_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(prep_su_t5, observed).
narrative_ontology:measurement(prep_su_t10, preparedness_transmission__husk_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement_basis(prep_su_t10, observed).
narrative_ontology:measurement(prep_su_t15, preparedness_transmission__husk_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement_basis(prep_su_t15, observed).
narrative_ontology:measurement(prep_su_t20, preparedness_transmission__husk_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement_basis(prep_su_t20, observed).
narrative_ontology:measurement(prep_su_t25, preparedness_transmission__husk_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement_basis(prep_su_t25, observed).
narrative_ontology:measurement(prep_su_t30, preparedness_transmission__husk_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(prep_su_t30, observed).
narrative_ontology:measurement(prep_su_t35, preparedness_transmission__husk_reading, suppression_requirement, 35, 0.48).
narrative_ontology:measurement_basis(prep_su_t35, observed).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__husk_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(prep_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_transmission kernel decomposes into three readings: husk_reading (this file) models the constraint as institutional theater persisting past its functional life; competence_reading models drills and inspections as live knowledge transmission; hybrid_reading models knowledge transmission as stratified (infrastructure competence high, coordination competence low). Each reading instantiates a different constraint with different ε, different victim sets, and different classification. The three readings are linked as coexisting positions held by different institutional seats. The ε-invariance principle requires separate stories because the referent (the standing arrangement under contest) is the same—the formal preparedness system—but the husk_reading evaluates it as a ritual performance with decayed underlying knowledge, yielding higher ε than the competence_reading would author. The three stories form a constraint family and should be analyzed together via network effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_transmission__husk_reading, powerless, 0.92).
constraint_indexing:directionality_override(preparedness_transmission__husk_reading, moderate, 0.74).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
