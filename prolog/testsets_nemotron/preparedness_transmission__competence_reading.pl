% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__competence_reading, []).

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
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Drills and Inspections as Live Exercised Knowledge
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   This constraint models the competence_reading of the
 *   preparedness_transmission kernel: drills and inspections function as live
 *   exercised knowledge where each generation of practitioners re-validates
 *   capability through practice. The structural claim is that adaptive
 *   capacity remains high — inspectors recognize novel failure signatures,
 *   drill participants improvise effectively under scenario variation, and
 *   knowledge transmission occurs through active exercise rather than static
 *   documentation. This reading asserts genuine coordination function with
 *   minimal extraction: the constraint solves the collective-action problem
 *   of maintaining operational readiness across generations without imposing
 *   parasitic costs on participants. The beneficiary structure is broad and
 *   symmetric — civil defense agencies, responders, infrastructure operators,
 *   and vulnerable populations all gain from maintained competence. No victim
 *   class exists because participation is not coercively extracted; the
 *   arrangement persists because it works, not because alternatives are
 *   suppressed.
 *
 * KEY AGENTS:
 *   - civil_defense_agencies: Primary agenda_setter (institutional/biographical) — designs and mandates exercise cycles, maintains doctrinal frameworks
 *   - emergency_responders: Primary beneficiary and payer (organized/biographical/constrained) — invest training time, gain validated capability; exit constrained by professional identity and certification requirements
 *   - critical_infrastructure_operators: Beneficiary (institutional/generational/arbitrage) — gain validated interoperability and cascade-failure knowledge; can substitute private exercises but lose cross-sector integration
 *   - vulnerable_populations: Beneficiary (powerless/civilizational/trapped) — gain protection from maintained system competence; no exit, no voice in design
 *   - inspectors_and_exercise_controllers: Observer/beneficiary (organized/biographical/mobile) — develop novel failure signature recognition through repeated exposure; professional reputation tied to detection accuracy
 *   - drill_participants: Beneficiary/payer (moderate/biographical/constrained) — improvise under scenario variation; gain adaptive capacity, bear time/opportunity cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.22).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Drills and Inspections as Live Exercised Knowledge").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "disaster_risk_management/institutional_memory/civil_defense").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, '826ad08e-c48c-403e-b905-2b324b50ff4e').
narrative_ontology:cs_kernel_codification('826ad08e-c48c-403e-b905-2b324b50ff4e', distributed).
narrative_ontology:cs_authority_grounding('826ad08e-c48c-403e-b905-2b324b50ff4e', practice).
narrative_ontology:cs_interpretation_layer_present('826ad08e-c48c-403e-b905-2b324b50ff4e').
narrative_ontology:cs_reading_relation('826ad08e-c48c-403e-b905-2b324b50ff4e', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('826ad08e-c48c-403e-b905-2b324b50ff4e', preparedness_transmission__hybrid_reading, influences).
narrative_ontology:cs_axiom('826ad08e-c48c-403e-b905-2b324b50ff4e', foundational, adaptive_capacity_requires_live_exercise).
narrative_ontology:cs_axiom_status(adaptive_capacity_requires_live_exercise, holdable).
narrative_ontology:cs_axiom_grounding('826ad08e-c48c-403e-b905-2b324b50ff4e', adaptive_capacity_requires_live_exercise, empirically_contingent).
narrative_ontology:cs_axiom('826ad08e-c48c-403e-b905-2b324b50ff4e', foundational, knowledge_transmission_is_exercise_not_documentation).
narrative_ontology:cs_axiom_status(knowledge_transmission_is_exercise_not_documentation, holdable).
narrative_ontology:cs_axiom_grounding('826ad08e-c48c-403e-b905-2b324b50ff4e', knowledge_transmission_is_exercise_not_documentation, empirically_contingent).
narrative_ontology:cs_reference_frame('826ad08e-c48c-403e-b905-2b324b50ff4e', post_war_civil_defense_doctrine).
narrative_ontology:cs_drift_state('826ad08e-c48c-403e-b905-2b324b50ff4e', contemporary_complex_disaster_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('826ad08e-c48c-403e-b905-2b324b50ff4e', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, civil_defense_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, emergency_responders).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, critical_infrastructure_operators).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, vulnerable_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, drill_participants).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, emergency_responders).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, drill_participants).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, practice_validates_capability).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, knowledge_transmission_requires_exercise).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, adaptive_capacity_depends_on_variation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and mandate exercise cycles, maintain doctrinal frameworks, authorize inspection regimes. They control the constraint's parameters (frequency, scope, scenario variation) and capture its coordination value — validated interagency capability, institutional legitimacy, budget justification. Exit is arbitrage-grade: they can redesign the system, contract alternatives, or shift doctrine.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, civil_defense_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Invest substantial training time in drills and inspections; gain validated operational capability, professional certification, and interagency trust. The bargain is demanding but fair: high effort for high capability. Exit is constrained by professional identity (being a responder means maintaining this competence), certification requirements, and the lack of equivalent alternative validation pathways.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, emergency_responders, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, emergency_responders, payer).

% Participate in cross-sector exercises to gain validated interoperability knowledge and cascade-failure recognition. They benefit from the system's coordination function but can substitute private exercises and proprietary simulation — their exit is arbitrage-grade because they have resources to build alternatives, though they lose cross-sector integration value.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, critical_infrastructure_operators, beneficiary,
    institutional, generational, arbitrage, national).

% Gain protection from maintained system competence without bearing direct costs. They have no voice in exercise design, no exit from dependence on civil defense systems, and would pay the highest price if the knowledge chain broke. Their beneficiary status is structural — the constraint's function includes their protection — but they are not organized to advocate for it.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, vulnerable_populations, beneficiary,
    powerless, civilizational, trapped, national).

% Develop novel failure signature recognition through repeated exposure to varied exercise scenarios. Their professional reputation and career advancement depend on detection accuracy and adaptive judgment. They are neither pure beneficiaries nor payers — they are the constraint's sensory apparatus, gaining epistemic capital from the exercise while providing its validation function. Exit is mobile: they can transfer to other inspection regimes or advisory roles.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, inspectors_and_exercise_controllers, observer,
    organized, biographical, mobile, national).

% Improvise under scenario variation during exercises; gain adaptive capacity and situational judgment, bear time and opportunity costs. Participation is often mandatory for their role (volunteer responders, facility managers, community coordinators), making exit constrained. They experience the constraint as a genuine capability-building mechanism, not as extraction.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, drill_participants, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, drill_participants, payer).

% Offer simulation-only alternatives that claim equivalent capability validation at lower cost. They are structurally excluded from the core validation loop because the competence_reading's claim is that live exercise with physical stakes produces knowledge that simulation cannot. They would argue for substitution but are kept out by the constraint's epistemic standard.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, simulation_technology_vendors, excluded,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits tacit operational knowledge across generations of practitioners through live exercise — each generation re-validates capability by performing it under varied conditions, ensuring that adaptive capacity (recognizing novel failure signatures, improvising effectively) is maintained rather than merely documented.
% TRANSFER_FUNCTION: Moves training time, organizational attention, and resource commitment from participating agencies and individuals into validated adaptive capability that protects the entire civil defense system and the populations it serves. No concentrated financial extraction; the transfer is effort-for-capability across a broad beneficiary base.
% ABSENT_VOICES: Simulation technology vendors and advocates of certification-by-documentation are structurally excluded — they would argue that live exercise is unnecessarily costly and that equivalent competence can be validated through simulation or paper exercises. Their exclusion is epistemic: the constraint's claim is that the knowledge in question (adaptive capacity under genuine uncertainty) cannot be validated without live stakes.
% DISAPPEARANCE_RATIONALE: If live exercises and inspections vanished overnight, the intergenerational transmission of tacit operational knowledge would break. Within one generation, inspectors would lose novel failure signature recognition, responders would lose improvisation capability under genuine uncertainty, and the civil defense system would degrade to procedural compliance without adaptive capacity. The world would rearrange toward brittle, doctrine-bound response — exactly the husk_reading outcome.
% FOUNDING_PROBLEM: Post-WWII civil defense systems discovered that static plans and paper qualifications failed under novel disaster conditions — practitioners could not recognize failure signatures they had never seen exercised, and improvisation collapsed when scenarios deviated from doctrine. The founding problem was transmitting adaptive capacity, not just procedural knowledge, across generations of practitioners.
% FOUNDING_PROBLEM_CORROBORATION: After-action reports from novel disasters (e.g., 2011 Tohoku earthquake, 2021 European floods, 2023 Türkiye-Syria earthquakes) consistently show that agencies with live exercise regimes recognized novel cascades and improvised effectively, while those with simulation-only regimes failed. Independent disaster research centers (e.g., IRDR, UCL Warning Research Centre) corroborate that adaptive capacity correlates with live exercise frequency and scenario variation, not with documentation completeness. The husk_reading proponents contest this, citing cost and declining participation rates — but their corroboration comes from budget offices, not operational outcomes.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__competence_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__competence_reading_tests).
:- end_tests(preparedness_transmission__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.18) because the arrangement's primary function is coordination — solving the intergenerational transmission of tacit operational knowledge — and the costs borne by participants (training time, opportunity cost) are proportionate to the capability gains. Suppression is low (0.22) because the constraint does not rely on coercion to persist; alternatives (private exercises, simulation-only training, certification-by-documentation) exist but are inferior for maintaining adaptive capacity. Theater ratio is very low (0.12) because the exercises are the genuine article — each cycle produces real capability validation, not performance. Accessibility collapse is moderate (0.45) because while the live-exercise model dominates, alternative knowledge-transmission modes exist and are used where exercises are impractical. Resistance is moderate (0.35) because the arrangement demands genuine effort and some institutional actors periodically push for cheaper substitutes (tabletop-only, simulation-only), but these challenges are answered by demonstrated superiority of live exercise under novel conditions.
 *
 * PERSPECTIVAL GAP:
 *   From the civil_defense_agenda_setter seat, the constraint appears as essential coordination infrastructure — the only mechanism that reliably transmits tacit knowledge across generations. From the emergency_responder payer-beneficiary seat, it appears as a demanding but fair bargain: high effort for high capability. From the vulnerable_population trapped-beneficiary seat, it appears as an invisible shield — they bear no direct cost but would pay the highest price if the knowledge chain broke. The engine computes these as different effective extractions from the same base ε.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil defense agencies (agenda_setter, institutional power, generational horizon, arbitrage exit) sit at d ≈ 0.15 — they control the constraint and capture its coordination value. Emergency responders (beneficiary/payer, organized power, biographical horizon, constrained exit) sit at d ≈ 0.45 — they pay in training time but gain validated professional capability; constrained exit (professional identity, certification) prevents full beneficiary positioning. Critical infrastructure operators (beneficiary, institutional power, generational horizon, arbitrage exit) sit at d ≈ 0.1 — they gain cross-sector integration value and can substitute private exercises. Vulnerable populations (beneficiary, powerless, civilizational horizon, trapped exit) sit at d ≈ 0.05 — they are pure beneficiaries with no exit, but the constraint extracts nothing from them directly. Inspectors (observer/beneficiary, organized power, biographical horizon, mobile exit) sit at d ≈ 0.2 — they gain professional capital from detection accuracy. The derivation chain from beneficiary declarations + exit options produces these d values without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — transmitting operational knowledge across generations of practitioners — remains live (founding_problem_status: live). The competence_reading claims the arrangement continues to solve this problem effectively. Mandatrophy is not resolved because the problem persists and the solution remains functional. The constraint would be reclassified toward piton only if adaptive capacity metrics decayed while exercise frequency remained constant (rising theater_ratio), or toward tangled_rope if a victim class emerged (e.g., mandatory participation with no capability gain). Neither condition obtains in this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint instantiates the competence_reading of the contested kernel preparedness_transmission. The sibling readings are husk_reading (memorial ritual with hollowed operational knowledge) and hybrid_reading (stratified: engineering competence high, civilian coordination decayed). What structural elements differentiate this reading from its siblings?',
    'Compare the three readings'' victim/beneficiary structures, theater ratios, and resistance profiles. The competence_reading should show low theater_ratio, genuine coordination function, and adaptive capacity under scenario variation. The husk_reading would show high theater_ratio, absent victims but absent genuine beneficiaries. The hybrid_reading would show mixed metrics across domains.',
    'If the competence_reading''s metrics are indistinguishable from husk_reading, the kernel contest collapses — the ''live exercised knowledge'' claim is unfalsifiable. Distinct metric profiles validate the kernel decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural differentiation of competence_reading from husk_reading and hybrid_reading siblings').

omega_variable(
    adaptive_capacity_measurement,
    'Can adaptive capacity under scenario variation be measured independently of drill performance metrics, or does the measurement itself constitute the exercise?',
    'Track novel failure signature recognition rates across successive exercise cycles with injected unknown-unknown scenarios. Compare improvisation quality against baseline doctrinal response.',
    'If adaptive capacity cannot be measured without constituting the exercise, the constraint''s low theater_ratio becomes unfalsifiable — any observation of competence is the exercise itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_capacity_measurement, empirical, 'Epistemic circularity in measuring live exercised knowledge').

omega_variable(
    civilian_coordination_boundary,
    'Where does the competence_reading''s claim of high adaptive capacity end? Does it include civilian coordination, or is that domain ceded to hybrid_reading''s decay claim?',
    'Map drill participation and improvisation metrics across professional responders vs. civilian volunteers vs. general population. Identify the boundary where scenario variation produces effective improvisation vs. breakdown.',
    'If civilian coordination is within this reading''s scope and shows decay, the competence_reading is over-claimed and hybrid_reading captures reality. If civilian coordination is outside scope, the readings coordinate different domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_coordination_boundary, conceptual, 'Scope boundary between competence_reading and hybrid_reading on civilian coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(prep_tr_t5, preparedness_transmission__competence_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__competence_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(prep_tr_t15, preparedness_transmission__competence_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__competence_reading, theater_ratio, 20, 0.12).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__competence_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(prep_be_t5, preparedness_transmission__competence_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__competence_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement(prep_be_t15, preparedness_transmission__competence_reading, base_extractiveness, 15, 0.17).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__competence_reading, base_extractiveness, 20, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__competence_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(prep_su_t5, preparedness_transmission__competence_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement(prep_su_t10, preparedness_transmission__competence_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(prep_su_t15, preparedness_transmission__competence_reading, suppression_requirement, 15, 0.21).
narrative_ontology:measurement(prep_su_t20, preparedness_transmission__competence_reading, suppression_requirement, 20, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__competence_reading, 0.08).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__hybrid_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, civil_defense_infrastructure_investment).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, interagency_exercise_interoperability).

% DUAL FORMULATION NOTE:
% This constraint is one member of the preparedness_transmission constraint family (kernel_id: preparedness_transmission). The competence_reading claims live exercised knowledge with high adaptive capacity (low ε, low theater). The husk_reading claims memorial ritual with hollowed knowledge (high theater, no genuine beneficiaries). The hybrid_reading claims stratified competence (mixed metrics by domain). All three share the same referent (the drill/inspection system) but author different ε values — this is the ε-invariance principle in action: different readings of the same kernel instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
